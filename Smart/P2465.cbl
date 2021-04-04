      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                                         P2465.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *REMARKS.
      *
      * This is the personal client create program from the submenu.
      * The client number will be entered on the submenu or
      * automatically allocated. Data entered on the screen will be
      * written to the CLTS file.
      * The new client entered will default to alive and the status of
      * the client is active (AC).
      *
      ******************Enhancements for Life Asia 1.0****************
      *
      * In the majority of the Asian countries, an ID number is
      * generally mandatory when applying for a life insurance
      * policy. The following amendments have been made in the 2000
      * section to validate  the ID  when it has been entered.
      *
      * (i) Table T3645 will specify whether the ID number is
      *     mandatory.
      *
      *
      ****************************************************************
      *****************************************************
      *              AMENDMENT  HISTORY                   *
      *****************************************************
      * DATE.....   BY..   AMENDMENT.........................  NUMBER
      *
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx    NNN
      * 13/07/88    K.B.   UK VERSION - 02
      *
      * 22/03/95    M.A.   VERSION 4 - Release 9503.
      *                    Comments 001 to 024 removed from
      *                    amendment history.
      *
      * 24/03/94    G.P.   AQR 4972. New fields added are :      025
      *                       S2465-START-DATE - Inception date
      *                       S2465-ETHORIG    - Ethnic Origin
      *                       - Default Language to System.
      *                       - Make NRIC/CATEGORY fields mandatory.
      *                       -Consistent use of indicators!
      *                       -Clear CAPITAL field on CLTS.
      *                       -SECUITYNO and STATCODE fields not
      *                        mandatory.
      *                       -Address lines must be edited the
      *                        same as those in P2472,i.e.first
      *                        letter of each address line must
      *                        be uppercase.
      *                       -Must display all details for alternate
      *                        address and alias type of clients.
      *                       -Check if any errors exist after calling
      *                        screen IO.
      *
      * 28/04/94    G.P.      AQR 4972                           026
      *                       When in enquiry allow switching
      *                       to S2500 only if there is a '+' in
      *                       the salary history box.
      *
      * 29/04/94    G.P.      AQR 4972                           027
      *                       Update the role flags on Client
      *                       file when an alternate address or
      *                       alias record is deleted and the
      *                       'Master' client record has no more
      *                       corresponding alternate or alias
      *                       records.
      *
      * 05/10/94    S.P.      AQR 4972                           028
      *                       Given name should be mandatory.
      *                       Allow '#' in address fields.
      *                       Salutation should be mandatory.
      *
      * 05/02/95    CJS.      AQR 5764                           029
      *                       SCREEN IS NOW A WINDOW.
      *
      * 26/10/94    M.A.   AQR 5514.                             030
      *                    Address validation altered to use
      *                    rules held on table T2241
      *
      * 02/12/94    J.O.   AQR 5514.                             031
      *                    Remove hardcoded 'R' default.
      *                    Replace hardcoded Y/N coding with
      *                    Space/Non-Space.
      *
      * 16/02/95    J.O.   AQR 5514.                             032
      *                    The character validation employed
      *                    for the name and address used hardcoded
      *                    character arrays in this program.  These
      *                    have been replaced by reading T3716 with
      *                    a key of Language:Countrycode.
      *
      * 11/04/95    R.C.   AQR 5792.                             033
      *                    Address validation altered to allow
      *                    three consecutive letters - e.g
      *                    Janssstrat.
      ***********************************************************************
      *                                                                     *
      * ......... New Version of the Amendment History.                     *
      *                                                                     *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *
      * 12/10/95  01/01   A05912       Christopher Dobson
      *           Two part AQR:
      *           i) Validation performed by the screen IO is now
      *           negated when in enquiry mode.
      *           ii) Validation of the country code performed
      *           incorrectly. A WSAA- variable was set up in 1000-
      *           section to contain the original value of the client
      *           country code. This WS field is then used instead of
      *           CLTS-CTRYCODE for validating address prompts and format
      *           It is updated after each validation and compared with
      *           the screen code to check for any change.
      *           This must also be done for the sign-on language which
      *           could be changed using one of the language switching
      *           function keys.
      *
      * 24/10/95  01/01   A06259       Rob Yates
      *           The validation of the allowed characters in Name Fields
      *           by accessing items on T3716 has been changed. It first
      *           looks for the combination of language and country code
      *           then language and *** (default of country code) and
      *           finally **** (default of language + country code) if
      *           not present.
      *           An additional change is to make the input of language
      *           code mandatory if spaces in field.
      *
      * 27/10/95  01/01   A06289       Rob Yates
      *           The two fields Mailing and Direct Mailing can now be
      *           selected independently of each other.
      *
      * 30/10/95  01/01   A06288       Rob Yates
      *           The first letter of all names and addresses can now be
      *           entered as either upper or lower case letters instead of
      *           just upper case.
      *
      * 28/11/95  01/01   A06083       Andrew Wall
      *
      *           Delete call to CLTSIO was missing when trying to
      *           delete a client.  This meant that the client record
      *           still existed after a delete client was requested.
      *           This call has now been added.
      *
      * 08/12/95  01/01   A05918       Andrew Wall
      *
      *           After entering an alternate address, the address type
      *           used to update the alternate address was coming from
      *           the primary address.  Ensure that the address type is
      *           not updated along with the rest of the common data.
      *
      *                                                                     *
      * 10/02/96  01/01   R96REA       Dave Welsh                           *
      *           Retrofit of Singapore Reassurance development.       ??   *
      *                                                                     *
      * 25/03/96  01/01   R96REA       Rachel Cartwright                    *
      *           Amendment to allow for creation of sub-standard           *
      *           retention record by placing an 'X' in RACIND.             *
      *                                                                     *
      * 15/05/96  01/01   D96NUM       Fiona Martin                         *
      *           Recompiled because of field changes for the Monetary
      *           field project (DECGRSAL).
      *                                                                     *
      ***********************************************************************
      *                                                                     *
      * ......... New Version of the Amendment History.                     *
      *                                                                     *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *
      * 09/07/96  01/01   CAS1.0       Sunil Patel
      *           Enhancements for ID Number. The field SECUITYNO is
      *           used as the ID Number for Asian Clients.This ID
      *           may require validation. This will be done by calling
      *           table T3645 which may or may not contain a validation
      *           subroutine.
      *
      * 23/07/96  01/01   CAS1.0       Dominic Dunbar
      *           A new field, ZRMANDIND hs been added to Table T3645.
      *           This field is used to determine whether the NI Number
      *           is mandatory or not. The relevant validation for this
      *           field has been put into this Program.
      *
      * 15/08/96  01/01   CAS1.0       Cat Chiu
      *                   To allow '/' to be entered for client
      *                   name.
      *
      * 27/08/96  01/01   CAS1.0       TONY TANG
      *           REMOVE LITERALS 'STREET', LINE 1, LINE 2, LINE 3
      *           REPLACE WITH 'ADDRESS'.
      *
      * 30/09/96  01/01   RA9606       David ODriscoll                      *
      *           Refit of A9606 (Re-compile only req'd).                   *
      *           Includes base work units: A06246, A05978, A06084.         *
      *                                                                     *
      * 15/11/96  01/01   GRP2.0       Ariane Durr                          *
      *           ID Number literal and default country code are now        *
      *           taken from T3711                                          *
      *           Use S2465-NMFMT instead of S2465-ETHORIG
      *                                                                     *
      * 09/01/97  01/01   GRP2.0       Doris Yu                             *
      *           Current system uses the address country code to get the   *
      *           IC No. Validation routine from T3645. It should be using  *
      *           the Nationality code.                                     *
      *                                                                     *
      * 03/02/97  01/01   GRP2.0       Ronald Macarulay                     *
      *           During Client Inquiry, ID text not displayed, to move??   *
      *           reading of T3711 before checking for action.              *
      *                                                                     *
      * 12/02/97  01/01   FUPLET       Dominic Dunbar                       *
      *           Doctor determined by an indicator on the screen.
      *                                                                     *
      * 13/03/97  01/01   WLI.P2       YUNI                                 *
      *           S2465-SALUTL is replaced by S2465-SALUT              ??   *
      *                                                                     *
      * 29/04/97  01/01   CAS2.0       Dominic Dunbar                       *
      *           Reversal of WLI.P2 above as the "Enhanced Windowing" ??   *
      *           facility allowed only CAPITAL LETTERS to be entered  ??   *
      *           as a salutation. Apologies if the dates are askew,   ??   *
      *           but the original code was not commented out, but     ??   *
      *           deleted.                                             ??   *
      *           Only that part of the amendment concerning the            *
      *           Salutation problem has been reversed.                     *
      *                                                                     *
      * 02/05/97  01/01   CAS2.0       Ariane Durr                          *
      *           Compile                                                   *
      *                                                                     *
      * 28/11/97    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      *                                                                     *
      * 18/11/97  01/01   FPS30        Rob Cooper                           *
      *                                                                     *
      *           Retrofit following amendments for POLISY SE Asia Ver 3.0  *
      *                                                                     *
      *           CN005                                                     *
      *           Add options:                                              *
      *           - 'Additional' for user to enter additional client info   *
      *             e.g. internet address, mobile phone, etc.               *
      *                                                                     *
      * 07/01/98  01/01   PSE30        Lew Kheep Seng                       *
      *           Client Name Expansion.                                    *
      *                                                                     *
      * 30/04/98  01/01   PSE309       Jessico                              *
      *           Protect RACR Indicator if no record exists                *
      *                                                                     *
      * 23/06/98  01/01   V4F001       Jolene Ng - ISD                      *
      *           Modify for 001 to 005 - Personal Client Details.          *
      *                                                                     *
      * 14/09/98  01/01   V4F001       Yong Kee Jee - CSC                   *
      *           For Dummy Security No numeric is not allowed              *
      *                                                                     *
      * 16/03/99  01/01   V4F012       Ali                                  *
      *           Additional 4 lines for special characters.                *
      *                                                                     *
      * 18/03/99  01/01   <FSA566>    LEW
      *           Call Subroutine(Subroutine name from TR393) to
      *           default and validate values according to TR393.
      *
      * 04/11/99  01/01   V5L001       Josephine Chiam                      *
      *           If the characters read from T3716 is spaces, skip         *
      *           the checking of valid characters.                         *
      *
      *           Default the language kept in CLTS as WSSP-LANGUAGE.
      *
      * 24/11/99  01/01   V5L001       Josephine Chiam                      *
      *           Skip name & address validations if Alternative            *
      *           language is used and it supports DBCS.                    *
      *                                                                     *
      * 15/12/99  01/01                Ch'ng Chin Tat (Retrofit A07142)     *
      * 03/07/98  01/01   A07142       Carlos Rivas                         *
      *           Rules in Table T2241 has been changed. The new valid ru-  *
      *           les are: M, O or N (M: Mandatory, O: Optional, N: Not to  *
      *           be used). The effect of these are reflected in the way    *
      *           validation of address lines are now done.                 *
      *
      * 13/06/00  01/01   MLS001       Boboy                                *
      *           Get the ID text from TR386
      *
      * 28/06/00  01/01   FAS1115      Boboy                                *
      *           Upon returning from the 'S/S Retentions' check box,
      *           most of the fields become unprotected during Inquiry
      *           Also, when F12 is pressed during client create system
      *           still perform validations.
      *
      * 24/07/00  01/01   MLS001       TYW                                  *
      *           For non-English language and DBCS, set CLTS-INITIAL
      *           as spaces
      *                                                                     *
      * 02/10/00  01/01   FA1162       Yoke Wah                             *
      *           Get the default name format from TR393 table              *
      *
      * 15/11/00  01/01   MLS002       Chu Kok Kuan
      *           To read table TR386 using generic item key instead
      *           of using combination of language and program ID.
      *
      * 13/12/2000 01/01  FA1226       Cynthia Lim
      *           Modify to the TR386REC changes.
      *
      * 09/11/00  01/01   PS1204      Boboy                                 *
      *           Retrofitted from OAC:                                     *
      *           01/01   OAC45        Tan Mui Kiang - CSC                  *
      *    PR555  Fix client role update problem after F9 to add Alt.       *
      *           Addr or F11 to add Client Alias followed by Additional    *
      *           Details                                                   *
      *    (Retrofited by Yong Kee Jee on 15/12/2000)
      *                                                                     *
      * 16/05/01  01/01   ICA011       YONGYUT                              *
      *           Grant or revoke access to a client under SDA user         *
      *                                                                     *
      * 24/06/01  01/01   V6F104       Ana Edy Poerwati                     *
      *           Check ID duplication if T3645-ZRMANDID = 'Y' and the      *
      *           T3645-ZRNISUBR = Spaces.                                  *
      *                                                                     *
      * 11/09/01  01/01   FA1971       Kok Chiew Ping
      *           Update the ENRL records when the client name has been
      *           changed.
      *
      * 10/10/01  01/01   V62P07       YONGYUT                              *
      *           Validates work permit holders in Singapore (FIN Number)   *
      *                                                                     *
      * 13/11/01  01/01   V62P07       WORACHART                            *
      *           NRIC/FIN Number Validation                                *
      *                                                                     *
      * 14/02/02  01/01   FA2360       WORACHART                            *
      *           RETROFIT  06/02/02  01/01   CSC023       Leekimpau        *
      *           do not validate the fields during inquiry                 *
      *                                                                     *
      * 10/04/02  01/01   PS2530       Ong Kean Guan - AXA Malaysia         *
      *           Recompile due to change of CLSCVALREC.                    *
      *                                                                     *
      * 16/04/02  01/01   FA2530       Ong Kean Guan - AXA Malaysia         *
      *           Recompile - Replace tag PS2530.                           *
      *                                                                     *
      * 06/05/02  01/01   V63P31       YONGYUT                              *
      *           Include S/tax field                                       *
      *                                                                     *
      * 05/06/02  01/01   FA2737       YONGYUT                              *
      *           -Remove redundant double checking SALTUL field because    *
      *            this field is controlled by TR393 and validate with      *
      *            subroutine CLSCVAL                                       *
      *           -After setting warning message for invalid ID, it should  *
      *            go to 2000-CONT                                          *
      *                                                                     *
      * 09/07/02  01/01   FA2773       Cynthia Lim Sin Bee                  *
      *           When business object, skip the warning error msg,    ??   *
      *           RF01 to display.                                          *
      *                                                                     *
      * 18/09/02  01/01   V64F04       Lim Ming Jern                        *
      *           Client Blacklist.                                         *
      *           Added a new field CLTSTAT (Client Status)                 *
      *                                                                     *
      * 29/01/03  01/01   V64F04       Soon Chuan Heok                      *
      *           Recompile program due to changes to its screen.           *
      *                                                                     *
      * 12/03/03  01/01   PCPRT2       Saw Geok Tin                         *
      *           Recompile.                                                *
      *                                                                     *
      * 27/03/03  01/01   V64F04       Lee Kim Pau                          *
      *           unprotect cltstat when modify                        ??   *
      *                                                                     *
      * 08/04/03  01/01   V64F04       Nagamuthu S.Ramaiah                  *
      *           Retrofit from AXA (R5024S Boboy - 03/04/03)               *
      *           Default Client Status to 'AC' during client creation      *
      *           from the policy header or from the agent maintenance      *
      *           screens.                                                  *
      *                                                                     *
      * 10/06/03  01/01   V65F10       Saw Geok Tin                         *
      *           MIS Agency Enquiry                                        *
      *           - Call new routine CNENQUP to update CNEQ if there        *
      *             is a change in client name or ID No.                    *
      *                                                                     *
      * 21/08/03  01/01   V65F10       Lew                                  *
      *           MIS Agency Enquiry                                        *
      *           - Call new routine AGCNQUP to update AGCN if there        *
      *             is a change in client name.                             *
      *                                                                     *
      * 25/09/03  01/01   FA2751       Saw Geok Tin                         *
      *           Retrofit changes by YONGYUT (12/06/03) &                  *
      *           Heng Cheng Kin (17/06/03)                                 *
      *           Note: the change by YONGYUT is redundant but retrofitted  *
      *                 for reference
      * 12/06/03  01/01   FA2751       YONGYUT                              *
      *           No validation upper case character of salutation for      *
      *           the language is 'T'                                       *
      * 17/06/03  01/01   FA2751       Heng Cheng Kin                       *
      *           The issue is on Thai Character. In Salutation, there is   *
      *           a check on Upper Case routine. The routine cannot         *
      *           difference Thai Character. The Salution table item key    *
      *           is always Upper Case for English. Therefore the upper     *
      *           case routine is redundancy. The fix by Yongyut is ignore. *
      *           Note: The language in Client is used to determine type    *
      *           of document language to be printed. This is an enhancement*
      *           for Axa.                                                  *
      *                                                                     *
      * 10/03/04  01/01   FA3226       Saw Geok Tin                         *
      *           Wrong error message displayed for invalid addr format     *
      *                                                                     *
      * 07/07/04  01/01   FA3292       Nancie lin - FSG Taiwan              *
      *       a)  Update ENRL when adding client to update the Client  ??   *
      *           Name in ENRL                                              *
      *                                                                ??   *
      *       b)  While checking the duplicate ID, ignore the client   ??   *
      *           for alternate address.                                    *
      *                                                                     *
      * 12/07/04  01/01   FA3358       Tan Boo Kean                         *
      *           Retrofit from Jerneh Insurance                            *
      *           Function keys 9 and 11 are not available under Enquiry    *
      *           mode; return error message instead if System Error.       *
      *                                                                     *
      * 11/05/06  01/01   FA3869       Lim Sin Bee Cynthia - CSC Mala       *
      *        - Protect Client status field when in modify client          *
      *          submenu action.                                            *
      *        - Not allowed to enter death date if the client status       *
      *          is not 'DN'.                                               *
      *                                                                     *
      * 12/07/06  01/01   FA3869       Chu Kok Kuan - CSC Malaysia          *
      *           To protect date of death if client status
      *           is not 'DN' during client modification.                   *
      *                                                                     *
      * 10/01/07  01/01   FA4126       David Ong - Malaysia                 *
      *           Retrofit from ING/Malaysia fixes.                         *
      *           - CSCF25 by David Ong.                                    *
      *           Obey T3645 rules with regards to duplicate IC.            *
      *                                                                     *
      * 30/01/07  01/01   V71F10       Gang Liu/ASIA/CSC (Beijing)          *
      *           To include switch box for access Bankrutcy details.       *
      *                                                                     *
      * 03/05/07  01/01   V72F01       Chu Kok Kuan
      *           Client new format enhancement.                            *
      *                                                                     *
      * 24/09/08  01/01   V73F02       Dylan Jenkins - CSC UK               *
      *           History records now kept for CLNTpf, SALHpf, SOINPF,      *
      *           and CLEXpf.                                               *
      *                                                                     *
      * 09/06/09  01/01   V74F03       Xu Chen/ASIA/CSC (China)             *
      *           Add a new switching 'CLPRFIND' for client profiling       *
      *           and various proof for Anti Money Laundering purpose.      *
      *                                                                     *
      * 18/06/09  01/01   FA5150       Xu Chen/ASIA/CSC (China)             *
      *           When perform 'REWRT',encounter bomb prob. Add 'READH'     *
      *           function to fix it.                                       *
      *                                                                     *
      * 07/07/09  01/01   V74F03       Fred Chi Ngan Lee/FSG/CSC (Hon       *
      *           Add 3 more parameter when calling IC Validation           *
      *           Allow to display invalid sex for IC validation            *
      *                                                                     *
      * 23/07/09  Retrofitted by Fred Chi Ngan Lee using AQR FA4547         *
      *           01/01   FA4547       CSC - Aidan Galligan                 *
      *           Added code to ensure indicator fields are displayed       *
      *           properly at bottom of screen.                             *
      *                                                                     *
      *           01/01   FA4547       Kristin Mcleish                      *
      *           When all checkboxes have been selected, the screens       *
      *           should be displayed in the order that the options         *
      *           appear on the screen going from left to right.            *
      *                                                                     *
      * 11/09/09  Retrofitted by Fred Lee/FSG/CSC (Hong Kong)               *
      *           01/01   FA4629       Saw Hoong Ong/FSG/CSC (Malaysia)     *
      *           For Action J - Update Client Status, function key         *
      *           F9 & F11 should not allowed. Protect Salutation field     *
      *                                                                     *
      * 15/12/09  01/01   V75F01       Saw Hoong Ong/FSG/CSC (Malaysia)     *
      *           Secured Data Access                                       *
      *          -For Client Creation, call BLDENRL in order to create USAE *
      *           Secured Data Access Entity & ENRL Entity Relationship     *
      *           records.                                                  *
      *          -For Client Modification, if there is a change of client   *
      *           name & ID, update ENRL accordingly.                       *
      *                                                                     *
      * 22/09/10  01/01   FA5050       Nath Mathmaluwe/FSG/CSC (Singa       *
      *           Help Desk #: 346726-XS10
      *           When user is creating a new client by prompting           *
      *           Contract Header's Contract Owner field (Session-A)
      *           and the same time user go to another session
      *           (Session-B) and go to Client Maintenance submenu.
      *           In the Submenu, user copies session- A's Client           *
      *           Number in to Client Number field and select Option A
      *           to create a new client. Finish Session-A's client
      *           creation and move to Session-B to finish the
      *           client creation.                                          *
      *           Result:
      *           System allows creating different clients with same
      *           client number. Since Session-A invoked client
      *           creation from contract header, it skips the soft lock
      *           creation and allows Session-B to go ahead and create
      *           a different Client with same number.                      *
      *           Solution:
      *           In 2000-SCREEN-EDIT section, during the client
      *           creation, check client number is already exists in
      *           the Client Master file. If so, display an error
      *           message.                                                  *
      *                                                                     *
      * 27/09/10  01/01   V76F10       Eubon Gicain/ASIA/CSC (Singapo       *
      *           Revert changes made under V73F02. History of records      *
      *           will now be handled using the SMART Audit Trail.          *
      *                                                                     *
      *           For Delete Option, record will not be physically deleted  *
      *           instead valid flag will be set to '2'.                    *
      *                                                                     *
      * 01/03/11  01/01   V76G57       Nath Mathmaluwe (FSG Singapore       *
      *           Re-Compile only.                                          *
      *                                                                     *
      * 24/08/11  01/01   FA5288       Josefa Fransisco De Jesua FSG/       *
      *           HelpDesk#515928-XS11.                                     *
      *           Record of client is not deleted properly when user        *
      *           press F12 to abandon client creation.                     *
      *                                                                     *
      *           Solution:                                                 *
      *           Delete the client record in CLTS, CLTN when abandon       *
      *           during client creation.                                   *
      *                                                                     *
      * 19/10/12  01/01   FA5297       Foong Leng Lee - CSC Singapore       *
      *          -Revert change under FA5150 since program already          *
      *           uses UPDAT instead of REWRT.                              *
      *                                                                     *
      *          -Not pass validflag '1' when creating CLEX record due to   *
      *           V76F10 change. Fix it.                                    *
      *                                                                     *
      * 16/09/13  01/01   GAPPH1       Thanh Do                             *
      *           Add Location/PostCode to Client Screen to auto            *
      *           getting Ward, Dist. and City.
      *           Check Maximum Client Age created is 100 years of age.
      *                                                                     *
      * 12/12/13  01/01   GAPPH2       Thanh Do                             *
      *           Set Mandatory for ID Date & ID Place.                     *
      *           Remove Mandatory for SOE field.
      *                                                                     *
      * 19/12/13  01/01   GAPPH2       Tuan Le                              *
      *           INITIALS S2465-IDDATE.                                    *
      *                                                                     *
      * 15/05/14  01/01   GAPPH2       Tuan Le                              *
      *           Print letter from transaction modify.                     *
      *           Create new staff indicator on client screen:
      *            + 'Y' for Staff and NO print billing notice
      *            + 'N' for Staff and print billing notice.
      *            + Defaut is Blank for normally cases.
      *                                                                     *
      * 18/05/16  01/01   PHFX02       Phi Tran - IT DEV                    *
      *           Skip print Letter some Field of GENSSW ='X'.              *
      *                                                                     *
      * 02/08/16  01/01   NB006        Tuyet Huynh IT - DEV                 *
      *           SET DEFAULT VALUE 'E' FOR FIELD LANGUAGE                  *
      *                                                                     *
      * 08/09/16  01/01   NB006        Phi Tran - IT DEV                    *
      *           Recompile Only.                                           *
      *                                                                     *
      * 10/10/16  01/01   DA002        Tuyet Huynh IT - DEV                 *
      *           ID No Modified Authoried User                             *
      *                                                                     *
      * 15/11/16  01/01   PS027        Tuyet Huynh IT - DEV                 *
      *           Update Off & H/s phone from S2472 screen                  *
      *                                                                     *
      * 16/01/17  01/01   PHE006       Thanh Do                             *
      *           Re-arrange screen fields as requested. Recompile only     *
      *                                                                     *
      * 17/02/17  01/01   NB008        Tuyet Huynh IT - DEV                 *
      *           Check phone number.                                       *
      *                                                                     *
      * 24/08/17  01/01   NB019        Thanh Do                             *
      *           Check duplicate address for action: Create / Modify.      *
      *                                                                     *
      * 05/04/18  01/01   PS013        Tuyet Huynh IT - DEV                 *
      *           Skip Validation paragraph when F9.                        *
      *                                                                     *
      * 17/04/18  01/01   PS012        Tuyet Huynh IT - DEV                 *
      *           Extract letter when modified Givenname, Surname           *
      *           ,Birth date, Gender, Nationality, ID Number
      *           ,ID Issued Place, ID Issued Date.
      *                                                                     *
      * 31/10/18  01/01   NB020        Phi Tran - IT DEV                    *
      *           Don't Validate Address.                                   *
      *                                                                     *
      * 20/11/18  01/01   DA014        Thanh Do                             *
      *           Name Input for Minor Peoples' Clients.                    *
      *                                                                     *
      * 28/05/20  01/01   PHE025       Thanh Do                             *
      *           Validate ID Date >= 01/01/1900.                           *
      *                                                                     *
      * 26/10/20  01/01   CLM14        Van Bao Tuyen - IT                   *
      *           Increase length ID Place from 30 to 100                   *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                               IBM-AS400.
       OBJECT-COMPUTER.                               IBM-AS400.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'P2465'.
       01  WSAA-VERSION                PIC X(02) VALUE '04'.
       01  WSAA-INTLMD                 PIC X(03).
       01  WSAA-INITIALS REDEFINES WSAA-INTLMD.
           03  WSAA-INT1               PIC X.
           03  WSAA-INT2               PIC X.
           03  WSAA-INT3               PIC X.
       01  WSAA-PTR                    PIC 9(02) COMP-3.
       01  PROG-COUNT                  PIC 9(02) VALUE ZERO.
      *
       01  WSAA-DN                     PIC XX     VALUE 'DN'.
       01  WSAA-INIT                   PIC X(01).                       <029>
       01  WSAA-VAL-IDDATE             PIC 9(08) VALUE 19000101.        <PHE025>
      *
       77  WSAA-X                      PIC S9(03) COMP-3 VALUE 0.
       77  WSAA-Y                      PIC S9(03) COMP-3 VALUE 0.
       01  FILLER.
           03  WSAA-SEC-PROG           PIC X(05) OCCURS 8.
      *
       01  WSAA-END-OF-CLRR            PIC X      VALUE 'N'.
           88  END-OF-CLRR                        VALUE 'Y'.
      *                                                                 <CN005>
       01  WSAA-CLEX-EXISTS            PIC X      VALUE 'N'.            <CN005>
           88  CLEX-EXISTS                        VALUE 'Y'.            <CN005>
      *                                                                 <CN005>
       01  WSAA-RACR-EXISTS            PIC X      VALUE 'N'.            <CN005>
           88  RACR-EXISTS                        VALUE 'Y'.            <CN005>
      *                                                                 <027>
       01  WSAA-UPDATE-CLNT            PIC X      VALUE 'N'.            <027>
      *                                                                 <027>
       01  WSAA-CLNTNUM                PIC X(08).                       <027>
      *                                                                 <027>
       01  WSAA-CLRROLE                PIC X(02).                       <027>
      *
       77  WSAA-ITEM-LENGTH            PIC 9(05) COMP-3 VALUE 8.
       77  WSAA-QCASE256               PIC X(10) VALUE 'QCASE256'.
       77  WSAA-QUSRSYS                PIC X(10) VALUE 'QUSRSYS'.
      *
       01  WSAA-CLTIND                 PIC X(01) VALUE ' '.
       01  WSAA-CHARACTERS.
      **** 03  WSAA-CHAR               PIC X  OCCURS 30.                <PSE30>
           03  WSAA-CHAR               PIC X  OCCURS 60.                <PSE30>
      *01  WSAA-INDEX                  PIC 99.                          <032>
      *01  WSAA-INDEXA                 PIC 99.                          <032>
      *01  WSAA-INDEXB                 PIC 99.                          <032>
      *01  WSAA-INDEXC                 PIC 99.                          <032>
       01  WSAA-INDEX                  PIC 9(03).                       <032>
       01  WSAA-INDEXA                 PIC 9(03).                       <032>
       01  WSAA-INDEXB                 PIC 9(03).                       <032>
       01  WSAA-INDEXC                 PIC 9(03).                       <032>
       01  WSAA-FIRST-TIME             PIC X(01)  VALUE 'Y'.            <PHFX02>
      *
       01  WSAA-CNT-LNG.                                                <030>
           03  WSAA-LNG                PIC X(01).                       <030>
           03  WSAA-CNT                PIC X(03).                       <030>
       01  WSAA-ITEM.                                                   <032>
           03  WSAA-LANGUAGE           PIC X(01).                       <032>
           03  WSAA-CTRYCODE           PIC X(03).                       <032>
       01  WSAA-SUB                    PIC 99.                          <030>
       01  WSAA-CHECK                  PIC X      VALUE 'N'.            <030>
           88  WSAA-OK                            VALUE 'Y'.            <030>
      *                                                                 <030>
      *01  WSAA-VALID-CHARS.                                       <032><025>
      **** 03  WSAA-ARRAY.                                         <032><025>
      ****     05  WSAA-ARRAY1             PIC X(27) VALUE         <032><025>
      ****                      "ABCDEFGHIJKLMNOPQRSTUVWXYZ.".     <032><025>
      ****     05  WSAA-ARRAY2             PIC X(35) VALUE         <032><025>
      ****                  "abcdefghijklmnopqrstuvwxyz '-()&#%.". <032><025>
      **** 03  WSAA-ARRAY-REDEF        REDEFINES WSAA-ARRAY.       <032><025>
      ****     05 WSAA-VALID-CHAR      PIC X OCCURS 62.            <032><025>
      *01  WSAA-NO-OF-CHAR             PIC 99 VALUE 62.            <032><025>
      ***                                                               <032>
      ***  Set up the character ranges from T3716 into the appropriate  <032>
      ***  variables within WSAA-ARRAY. NB.  total length = 3*40 = 120. <032>
       01  WSAA-VALID-CHARS.                                            <032>
           03  WSAA-ARRAY.                                              <032>
               05  WSAA-UPPER              PIC X(40).                   <032>
               05  WSAA-LOWER              PIC X(40).                   <032>
               05  WSAA-SPECIAL            PIC X(40).                   <032>
               05  WSAA-SPECIAL-2          PIC X(40).                   <V4F012>
               05  WSAA-SPECIAL-3          PIC X(40).                   <V4F012>
               05  WSAA-SPECIAL-4          PIC X(40).                   <V4F012>
               05  WSAA-SPECIAL-5          PIC X(40).                   <V4F012>
           03  WSAA-ARRAY-REDEF        REDEFINES WSAA-ARRAY.            <032>
   ****        05 WSAA-VALID-CHAR      PIC X OCCURS 120.                <032>
               05 WSAA-VALID-CHAR      PIC X OCCURS 280.                <V4F012>
   ****01  WSAA-NO-OF-CHAR             PIC 9(03) VALUE 120.             <032>
       01  WSAA-NO-OF-CHAR             PIC 9(03) VALUE 280.             <V4F012>
      *
       01  WSAA-COUNTRY                PIC X VALUE 'F'.                 <A05912>
           88 FIRST-REC                VALUE 'F'.                       <A05912>
           88 COUNTRY-CHANGED          VALUE 'Y'.                       <A05912>
      *                                                                 <A05912>
       01  WSAA-CLTS-CTRYCODE          PIC X(03).                       <A05912>
      *                                                                 <A05912>
       01  WSAA-LANG-CHANGED           PIC X(01).                       <A05912>
           88 LANG-CHANGED             VALUE 'Y'.                       <A05912>
                                                                        <A05912>
       01  WSAA-SIGNON-LANG            PIC X(01).                       <A05912>
                                                                        <FUPLET>
       01  WSAA-DOCTIND-STORE          PIC X(01) VALUE SPACES.          <FUPLET>
       01  WSAA-NAME-CHECK             PIC X(01) VALUE SPACES.          <FUPLET>
       01  WSAA-SECUITYNO              PIC X(24).                       <V62P07>
       01  WSAA-ID-CHANGED             PIC X(01).                       <V65F10>
       01  WSAA-IVID-SECUITYNO         PIC X(24).                       <V62P07>
       01  WSAA-OLD-SECUITYNO          PIC X(24).                       <V62P07>
       01  WSAA-NATLTY                 PIC X(03).                       <V62P07>
       01  WSAA-WARNING-COUNT          PIC 9(02).                       <V62P07>
       01  WSAA-CLNTSSN-FND            PIC X(01).                       <V62P07>
       01  WSAA-STAFFLAG-SAV           PIC X(01).                       <GAPPH2>
       01  WSAA-FIR-SECUITYNO          PIC X(24).                       <DA002>
       01  WSAA-FLAG-EXIST             PIC X(01) VALUE SPACES.          <NB008>
       01  WSAA-MB-WARNING             PIC X(01) VALUE SPACES.          <NB008>
       01  WSAA-OF-WARNING             PIC X(01) VALUE SPACES.          <NB008>
       01  WSAA-HS-WARNING             PIC X(01) VALUE SPACES.          <NB008>
       01  WSAA-MB-NUM                 PIC X(00016).                    <NB008>
       01  WSAA-OF-NUM                 PIC X(00016).                    <NB008>
       01  WSAA-HS-NUM                 PIC X(00016).                    <NB008>
       01  WSAA-SECURI-FND             PIC X(01) VALUE SPACES.          <NB008>
TDO    01  WSAA-ADDRESS-DUP            PIC X(01).                       <NB019>
       01  WSAA-ADDRLINE12             PIC X(61).                       <NB019>
       01  WSAA-CLNTLINE12             PIC X(61).                       <NB019>
       01  WSAA-ADDRLINE12-BK          PIC X(61).                       <NB019>
       01  WSAA-CLNTLINE12-BK          PIC X(61).                       <NB019>
       01  WSAA-ADDR-WARN              PIC X(01).                       <NB019>
                                                                        <NB008>
      *                                                                 <V62P07>
       01  WSAA-PAXMSG.                                                 <V62P07>
           03  WSAA-LANGUAGE-MSG       PIC X(01).                       <V62P07>
           03  WSAA-SERVICE-UNIT       PIC X(02) VALUE 'PS'.            <V62P07>
           03  WSAA-MSGID              PIC X(04).                       <V62P07>
      *                                                                 <A05912>
       01  WSAA-FIRST-LOOP             PIC X(01).                       <V73F02>
      *                                                                 <GAPPH1>
       01  WSAA-TABLENAME              PIC X(05).                       <GAPPH1>
       01  WSAA-TABLEITEM              PIC X(08).                       <GAPPH1>
       01  WSAA-LOCCODE.                                                <GAPPH1>
           03  WSAA-CITY               PIC X(02).                       <GAPPH1>
           03  WSAA-DIST               PIC X(03).                       <GAPPH1>
           03  WSAA-WARD               PIC X(05).                       <GAPPH1>
      *                                                                 <GAPPH2>
        01 WSAA-CLNT-DATA-SAVE.                                         <GAPPH2>
             03  WSAA-ADDRDESCS-SAV            .                        <GAPPH2>
               05  WSAA-ADDRDESC-SAV             PIC X(00010)           <GAPPH2>
                                                  OCCURS 05 .           <GAPPH2>
             03  FILLER REDEFINES WSAA-ADDRDESCS-SAV            .       <GAPPH2>
               05  WSAA-ADDRDESC-01-SAV          PIC X(00010)    .      <GAPPH2>
               05  WSAA-ADDRDESC-02-SAV          PIC X(00010)    .      <GAPPH2>
               05  WSAA-ADDRDESC-03-SAV          PIC X(00010)    .      <GAPPH2>
               05  WSAA-ADDRDESC-04-SAV          PIC X(00010)    .      <GAPPH2>
               05  WSAA-ADDRDESC-05-SAV          PIC X(00010)    .      <GAPPH2>
             03  WSAA-ADDRTYPE-SAV             PIC X(00001)    .        <GAPPH2>
             03  WSAA-BIRTHP-SAV               PIC X(00020)    .        <GAPPH2>
             03  WSAA-BRUPIND-SAV              PIC X(00001)    .        <GAPPH2>
             03  WSAA-CANFLAG-SAV              PIC X(00001)    .        <GAPPH2>
             03  WSAA-CLNTNUM-SAV              PIC X(00008)    .        <GAPPH2>
             03  WSAA-CLPRFIND-SAV             PIC X(00001)    .        <GAPPH2>
             03  WSAA-CLTADDRS-SAV             .                        <GAPPH2>
               05  WSAA-CLTADDR-SAV              PIC X(00030)           <GAPPH2>
                                                  OCCURS 05 .           <GAPPH2>
             03  FILLER REDEFINES WSAA-CLTADDRS-SAV             .       <GAPPH2>
               05  WSAA-CLTADDR-01-SAV           PIC X(00030)    .      <GAPPH2>
               05  WSAA-CLTADDR-02-SAV           PIC X(00030)    .      <GAPPH2>
               05  WSAA-CLTADDR-03-SAV           PIC X(00030)    .      <GAPPH2>
               05  WSAA-CLTADDR-04-SAV           PIC X(00030)    .      <GAPPH2>
               05  WSAA-CLTADDR-05-SAV           PIC X(00030)    .      <GAPPH2>
             03  WSAA-CLTDOBX-SAV              PIC S9(08)      .        <GAPPH2>
             03  WSAA-CLTDODX-SAV              PIC S9(08)      .        <GAPPH2>
             03  WSAA-CLTPCODE-SAV             PIC X(00010)    .        <GAPPH2>
             03  WSAA-CLTPHONES-SAV            .                        <GAPPH2>
               05  WSAA-CLTPHONE-SAV             PIC X(00016)           <GAPPH2>
                                                  OCCURS 02 .           <GAPPH2>
             03  FILLER REDEFINES WSAA-CLTPHONES-SAV            .       <GAPPH2>
               05  WSAA-CLTPHONE-01-SAV          PIC X(00016)    .      <GAPPH2>
               05  WSAA-CLTPHONE-02-SAV          PIC X(00016)    .      <GAPPH2>
             03  WSAA-CLTSEX-SAV               PIC X(00001)    .        <GAPPH2>
             03  WSAA-CLTSTAT-SAV              PIC X(00002)    .        <GAPPH2>
             03  WSAA-CTRYCODE-SAV             PIC X(00003)    .        <GAPPH2>
             03  WSAA-DIRMAIL-SAV              PIC X(00001)    .        <GAPPH2>
             03  WSAA-DOCNO-SAV                PIC X(00008)    .        <GAPPH2>
             03  WSAA-IDDATE-SAV               PIC S9(08)      .        <GAPPH2>
             03  WSAA-IDNOTXT-SAV              PIC X(00016)    .        <GAPPH2>
             03  WSAA-IDPLACE-SAV              PIC X(00030)    .        <GAPPH2>
             03  WSAA-IDPLACEXT-SAV            PIC X(00070)    .        <CLM14>
             03  WSAA-LANGUAGE-SAV             PIC X(00001)    .        <GAPPH2>
             03  WSAA-LGIVNAME-SAV             PIC X(00060)    .        <GAPPH2>
             03  WSAA-LSURNAME-SAV             PIC X(00060)    .        <GAPPH2>
             03  WSAA-MAILING-SAV              PIC X(00001)    .        <GAPPH2>
             03  WSAA-MARRYD-SAV               PIC X(00001)    .        <GAPPH2>
             03  WSAA-NATLTY-SAV               PIC X(00003)    .        <GAPPH2>
             03  WSAA-NMFMT-SAV                PIC X(00001)    .        <GAPPH2>
             03  WSAA-OCCPCODE-SAV             PIC X(00004)    .        <GAPPH2>
             03  WSAA-RACRIND-SAV              PIC X(00001)    .        <GAPPH2>
             03  WSAA-REXTRFLD-SAV             PIC X(00001)    .        <GAPPH2>
             03  WSAA-RINTERNET-SAV            PIC X(00050)    .        <GAPPH2>
             03  WSAA-RMBLPHONE-SAV            PIC X(00016)    .        <GAPPH2>
             03  WSAA-SALUTL-SAV               PIC X(00008)    .        <GAPPH2>
             03  WSAA-SCRTITLE-SAV             PIC X(00017)    .        <GAPPH2>
             03  WSAA-SECUITYNO-SAV            PIC X(00024)    .        <GAPPH2>
             03  WSAA-SERVBRH-SAV              PIC X(00002)    .        <GAPPH2>
             03  WSAA-SOE-SAV                  PIC X(00010)    .        <GAPPH2>
             03  WSAA-START-DATE-SAV           PIC S9(08)      .        <GAPPH2>
             03  WSAA-STATCODE-SAV             PIC X(00002)    .        <GAPPH2>
             03  WSAA-STATDSC-SAV              PIC X(00010)    .        <GAPPH2>
             03  WSAA-TAXFLAG-SAV              PIC X(00001)    .        <GAPPH2>
             03  WSAA-TSALUTSD-SAV             PIC X(00010)    .        <GAPPH2>
             03  WSAA-UK-PENSION-IND-SAV       PIC X(00001)    .        <GAPPH2>
             03  WSAA-VIP-SAV                  PIC X(00001)    .        <GAPPH2>
             03  WSAA-ZDOCTIND-SAV             PIC X(00001)    .        <GAPPH2>
             03  WSAA-ZPTCITY-SAV              PIC X(00002)    .        <GAPPH2>
             03  WSAA-ZPTDIST-SAV              PIC X(00003)    .        <GAPPH2>
             03  WSAA-ZPTWARD-SAV              PIC X(00005)    .        <GAPPH2>
      *                                                         <CL002> <V73F02>
       01  WSAA-CLTSKEY.
           COPY CLTSKEY.
      *
       01  WSAA-CLPRKEY.                                                <V74F03>
           COPY CLPRKEY.                                                <V74F03>
      *                                                                 <V74F03>
       01  WSAA-BATCKEY.
           COPY BATCKEY.
       01  WSAA-CLNTSSNKEY.                                             <V62P07>
           COPY CLNTSSNKEY.                                             <V62P07>
      *
       01  WSAA-TODAY                  PIC 9(08) VALUE 0.
       01  VALN-STATUZ                 PIC X(05).
       01  VALN-FUNCTION               PIC X(05).
       01  VALN-NAME                   PIC X(05) VALUE 'NAME'.
       01  VALN-ADDRESS                PIC X(05) VALUE 'ADDR'.
      *                                                                 <V4F001>
       01  WSAA-DUMMY-ID.                                               <V4F001>
           03  DUMMY-1                 PIC X     VALUE SPACE.           <V4F001>
           03  DUMMY-2                 PIC X     VALUE SPACE.           <V4F001>
           03  DUMMY-3                 PIC X     VALUE SPACE.           <V4F001>
           03  DUMMY-4                 PIC X     VALUE SPACE.           <V4F001>
           03  DUMMY-5                 PIC X     VALUE SPACE.           <V4F001>
      *
      *01  WSAA-TR386-KEY.                                      <MLS002><MLS001>
      **** 03  WSAA-TR386-LANG         PIC X(01).               <MLS002><MLS001>
      **** 03  WSAA-TR386-PGM          PIC X(05).               <MLS002><MLS001>
      **** 03  WSAA-TR386-ID           PIC X(03).               <MLS002><MLS001>
       01  WSAA-TR386-FSUCO-KEY.                                        <MLS002>
           03  WSAA-TR386-FSUCO-LANG   PIC X(01).                       <MLS002>
           03  WSAA-TR386-FSUCO-ID     PIC X(06) VALUE '***NID'.        <MLS002>
      *
       01  WSAA-SURNAME                PIC X(0060).                     <FA1971>
       01  WSAA-GIVNAME                PIC X(0060).                     <FA1971>
       01  WSAA-CLNTNAME               PIC X(0120).                     <FA1971>
      *
       01  WSAA-CNV-SURNAME            PIC X(0030).                     <FA1971>
       01  WSAA-CNV-GIVNAME            PIC X(0020).                     <FA1971>
       01  WSAA-CNV-CLNTNAME           PIC X(0047).                     <FA1971>
      *
       01  FORMATS.
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  CLNTSSNREC              PIC X(10) VALUE 'CLNTSSNREC'.
           03  CLPRREC                 PIC X(10) VALUE 'CLPRREC'.       <V74F03>
           03  CLRFREC                 PIC X(10) VALUE 'CLRFREC'.
           03  ENRLREC                 PIC X(10) VALUE 'ENRLREC'.       <FA1971>
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  CLEXREC                 PIC X(10) VALUE 'CLEXREC'.       <CN005>
           03  CLRRREC                 PIC X(10) VALUE 'CLRRREC'.
           03  SOINREC                 PIC X(10) VALUE 'SOINREC'.
           03  SALHREC                 PIC X(10) VALUE 'SALHREC'.
           03  RACRREC                 PIC X(10) VALUE 'RACRREC'.       <R96REA>
           03  BRUPREC                 PIC X(10) VALUE 'BRUPREC'.       <V71L02>
           03  SCLTENQREC              PIC X(10) VALUE 'SCLTENQREC'.    <GAPPH2>
           03  CLRRWINREC              PIC X(10) VALUE 'CLRRWINREC'.    <DA002>
           03  CLNTHSPREC              PIC X(10) VALUE 'CLNTHSPREC'.    <NB008>
           03  CLNTOFPREC              PIC X(10) VALUE 'CLNTOFPREC'.    <NB008>
           03  CLEXMBPREC              PIC X(10) VALUE 'CLEXMBPREC'.    <NB008>
           03  CLNTCHKREC              PIC X(10) VALUE 'CLNTCHKREC'.    <NB019>
           03  ZCLEREC                 PIC X(10) VALUE 'ZCLEREC'.       <CLM14>
      *                                                                 <NB008>
       01  TABLES.
           03  T3628                   PIC X(05) VALUE 'T3628'.
           03  T3571                   PIC X(05) VALUE 'T3571'.
           03  T3583                   PIC X(05) VALUE 'T3583'.
           03  T3645                   PIC X(05) VALUE 'T3645'.
           03  T3643                   PIC X(05) VALUE 'T3643'.         <V64F04>
           03  T2241                   PIC X(05) VALUE 'T2241'.         <030>
           03  T3716                   PIC X(05) VALUE 'T3716'.         <032>
           03  T3711                   PIC X(05) VALUE 'T3711'.
           03  TR393                   PIC X(05) VALUE 'TR393'.         <FSA566>
           03  T1680                   PIC X(05) VALUE 'T1680'.         <V5L001>
           03  TR386                   PIC X(05) VALUE 'TR386'.         <MLS001>
           03  TV010                   PIC X(05) VALUE 'TV010'.         <GAPPH1>
           03  TV011                   PIC X(05) VALUE 'TV011'.         <GAPPH1>
           03  TV012                   PIC X(05) VALUE 'TV012'.         <GAPPH1>
           03  TR384                   PIC X(05) VALUE 'TR384'.         <GAPPH2>
           03  TV087                   PIC X(05) VALUE 'TV087'.         <DA002>
      *
       01  ERRORS.
           03  B369                    PIC X(04) VALUE 'B369'.
           03  E110                    PIC X(04) VALUE 'E110'.
           03  E186                    PIC X(04) VALUE 'E186'.
           03  E315                    PIC X(04) VALUE 'E315'.
           03  E374                    PIC X(04) VALUE 'E374'.
           03  F073                    PIC X(04) VALUE 'F073'.
           03  F767                    PIC X(04) VALUE 'F767'.
           03  F991                    PIC X(04) VALUE 'F991'.          <V64F04>
           03  G616                    PIC X(04) VALUE 'G616'.          <FUPLET>
           03  G620                    PIC X(04) VALUE 'G620'.
      **** 03  F735                    PIC X(04) VALUE 'F735'.          <A06289>
           03  F994                    PIC X(04) VALUE 'F994'.
           03  G986                    PIC X(04) VALUE 'G986'.
           03  G985                    PIC X(04) VALUE 'G985'.
           03  G987                    PIC X(04) VALUE 'G987'.
           03  G983                    PIC X(04) VALUE 'G983'.
           03  G984                    PIC X(04) VALUE 'G984'.
           03  G602                    PIC X(04) VALUE 'G602'.
           03  H366                    PIC X(04) VALUE 'H366'.
           03  H093                    PIC X(04) VALUE 'H093'.
           03  D009                    PIC X(04) VALUE 'D009'.          <030>
           03  D036                    PIC X(04) VALUE 'D036'.          <032>
           03  D037                    PIC X(04) VALUE 'D037'.          <032>
           03  RF01                    PIC X(04) VALUE 'RF01'.          <CAS1.0>
           03  RF02                    PIC X(04) VALUE 'RF02'.          <CAS1.0>
      **** 03  E315                    PIC X(04) VALUE 'E315'.          <031>
           03  I032                    PIC X(04) VALUE 'I032'.          <V64F04>
           03  RGD6                    PIC X(04) VALUE 'RGD6'.          <V74F03>
           03  E057                    PIC X(04) VALUE 'E057'.          <FA5050>
           03  E032                    PIC X(04) VALUE 'E032'.
           03  W134                    PIC X(04) VALUE 'W134'.
           03  F975                    PIC X(04) VALUE 'F975'.          <GAPPH2>
           03  P129                    PIC X(04) VALUE 'P129'.          <DA002>
           03  EV91                    PIC X(04) VALUE 'EV91'.          <NB008>
           03  EV92                    PIC X(04) VALUE 'EV92'.          <NB008>
           03  EV93                    PIC X(04) VALUE 'EV93'.          <NB008>
           03  EV98                    PIC X(04) VALUE 'EV98'.          <NB019>
           03  D020                    PIC X(04) VALUE 'D020'.          <PHE025>
                                                                        <NB008>
      /
           COPY VARCOM.
      /
           COPY UPDCLTSREC.
      /
           COPY DATCON1REC.
           COPY DATCON3REC.
      /
           COPY SYSERRREC.
      /
      ***  COPY SCRNPARAMS.                                             <S9503>
      /
           COPY FSUPFXCPY.
      /
      ***  COPY S2465SCR.                                               <S9503>
      /                                                                 <CN005>
           COPY CLEXSKM.                                                <CN005>
      /                                                                 <CN005>
           COPY CLTSSKM.
           COPY ALOCNOREC.                                              <FA5288>
      *
           COPY CLRFSKM.
      *                                                                 <FUPLET>
           COPY CLPRSKM.                                                <V74F03>
      *                                                                 <V74F03>
           COPY ENRLSKM.                                                <FA1971>
      *
           COPY CLTRELNREC.                                             <FUPLET>
      *                                                                 <FUPLET>
           COPY CLNTRLSREC.                                             <FUPLET>
      *
           COPY CLRRWINSKM.                                             <FUPLET>
                                                                        <FUPLET>
           COPY CLNTSSNSKM.
      *
           COPY CLRRSKM.
      *
           COPY SOINSKM.
      *
           COPY SALHSKM.
      *
           COPY RACRSKM.                                                <R96REA>
      *                                                         <R96REA>
           COPY ITEMSKM.
      *
           COPY T3583REC.
      *
           COPY T3645REC.
      *                                                                 <030>
           COPY T2241REC.                                               <030>
      *                                                                 <032>
           COPY T3716REC.                                               <032>
      *                                                                 <CAS1.0>
           COPY T3711REC.
      *
           COPY T1680REC.                                               <V5L001>
      *
           COPY ZRIDCHKREC.                                             <CAS1.0>
      /
           COPY DESCSKM.
      /                                                                 <V71L02>
           COPY BRUPSKM.                                                <V71L02>
      /
           COPY OPSTATSREC.
      /
      *    COPY CHKPOSTREC.
           COPY PCODEREC.
      /
           COPY DBCSTRNCPY.                                             <FA1971>
      /
           COPY GENSSWREC.
      /
           COPY TR393REC.                                               <FSA566>
      /                                                                 <FSA566>
           COPY CLSCVALREC.                                             <FSA566>
      /
           COPY TR386REC.                                               <MLS001>
      /                                                                 <FSA566>
           COPY CLNENRLREC.                                             <ICA011>
      /                                                                 <ICA011>
           COPY MSGBOXREC.                                              <V62P07>
      /                                                                 <V62P07>
           COPY CNENQUPREC.                                             <V65F10>
      /                                                                 <V65F10>
           COPY AGCNQUPREC.                                             <V65F10>
      /                                                                 <V65F10>
           COPY BLDENRLREC.                                             <V75F01>
      /                                                                 <V75F01>
           COPY ENRLCLTSKM.                                             <V75F01>
           COPY ZLOCCHKREC.                                             <GAPPH1>
           COPY ZTBLCHKREC.                                             <GAPPH1>
           COPY SCLTENQSKM.                                             <GAPPH2>
      /                                                                 <GAPPH2>
           COPY TR384REC.                                               <GAPPH2>
           COPY LETRQSTREC.                                             <GAPPH2>
           COPY CLNTHSPSKM.                                             <NB008>
           COPY CLNTOFPSKM.                                             <NB008>
           COPY CLEXMBPSKM.                                             <NB008>
           COPY CLNTCHKSKM.                                             <NB019>
           COPY ZCLESKM.                                                <CLM14>
      /                                                                 <V75F01>
                                                                        <DA002>
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S2465SCR.                                               <S9503>
      *
           COPY WSSPCOMN.
      *
           COPY WSSPWINDOW.
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S2465-DATA-AREA      .   <S9503>
      *
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
       1001-INITIALISE.
      *
           MOVE 'Y'                    TO WSAA-FIRST-LOOP.              <V73F02>
           MOVE O-K                    TO WSSP-EDTERROR.                <GAPPH2>
           MOVE SPACES                 TO WSAA-MB-WARNING               <NB008>
                                          WSAA-OF-WARNING               <NB008>
                                          WSAA-HS-WARNING               <NB008>
                                          WSAA-MB-NUM                   <NB008>
                                          WSAA-OF-NUM                   <NB008>
                                          WSAA-HS-NUM                   <NB008>
                                          WSAA-SECURI-FND               <NB008>
                                          WSAA-FLAG-EXIST.              <NB008>
                                                                        <NB019>
TDO        MOVE SPACES                 TO WSAA-ADDRESS-DUP.             <NB019>
           MOVE SPACES                 TO WSAA-ADDR-WARN.               <NB019>
                                                                        <V73F02>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              MOVE 'N'                 TO WSAA-FIRST-LOOP               <V73F02>
              IF WSSP-FLAG = 'I'                                        FSA1115
                  MOVE 'Y'             TO S2465-SERVBRH-OUT (PR)        FSA1115
                                          S2465-LSURNAME-OUT (PR)       FSA1115
                                          S2465-CANFLAG-OUT (PR)        <GAPPH2>
                                          S2465-CLTADDR01-OUT (PR)      FSA1115
              END-IF                                                    FSA1115
              GO 1900-EXIT.
                                                                        <PHFX02>
           MOVE 'Y'                    TO WSAA-FIRST-TIME.              <PHFX02>
      *
           IF WSAA-TODAY = 0
              MOVE SPACES              TO DTC1-DATCON1-REC
              MOVE TDAY                TO DTC1-FUNCTION
              CALL 'DATCON1'  USING  DTC1-DATCON1-REC
              MOVE DTC1-INT-DATE       TO WSAA-TODAY
           END-IF.
                                                                        <GAPPH2>
      * Set up the batch key fields.                                    <GAPPH2>
           MOVE WSSP-BATCHKEY          TO WSKY-BATC-FILE-KEY.           <GAPPH2>
                                                                        <PHFX02>
      **** MOVE SPACES                 TO WSAA-CLNT-DATA-SAVE.  <PHFX02><GAPPH2>
           MOVE SPACES                 TO WSAA-CLNT-DATA-SAVE.          <PHFX02>
                                                                        <V5L001>
       1020-READ-T1680.                                                 <V5L001>
      *                                                                 <V5L001>
      *    Read T1680 - to identify if Alternative Language is used.    <V5L001>
      *                                                                 <V5L001>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <V5L001>
           MOVE ZERO                   TO ITEM-ITEMCOY.                 <V5L001>
           MOVE T1680                  TO ITEM-ITEMTABL.                <V5L001>
           MOVE WSSP-LANGUAGE          TO ITEM-ITEMITEM.                <V5L001>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <V5L001>
           MOVE READR                  TO ITEM-FUNCTION.                <V5L001>
                                                                        <V5L001>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <V5L001>
                                                                        <V5L001>
           IF ITEM-STATUZ           NOT = O-K                           <V5L001>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V5L001>
              PERFORM 600-FATAL-ERROR.                                  <V5L001>
                                                                        <V5L001>
           MOVE ITEM-GENAREA           TO T1680-T1680-REC.              <V5L001>
                                                                        <V5L001>
      *
       1050-ADDITION.
      *
      *    If at some previous point the user has been through this     <029>
      *    window but F3ed at a higher level window then we need to     <029>
      *    ensure the screen is initialised or we will have overlay     <029>
      *    problems. The problem is actually in MAINF and this is a     <029>
      *    temporary fix until MAINF is corrected.                      <029>
                                                                        <029>
           MOVE 'Y'                    TO WSAA-INIT.                    <029>
           MOVE 'F'                    TO WSAA-COUNTRY.                 <A06259>
           MOVE 'N'                    TO WSAA-LANG-CHANGED.            <A06259>
           MOVE SPACES                 TO WSAA-SECUITYNO                <V62P07>
                                          WSAA-IVID-SECUITYNO           <V62P07>
                                          WSAA-OLD-SECUITYNO            <V62P07>
                                          WSAA-NATLTY.                  <V62P07>
           MOVE VRCM-MAX-DATE          TO S2465-IDDATE.                 <GAPPH2>
           MOVE 1                      TO WSAA-WARNING-COUNT            <V62P07>
           MOVE SPACES                 TO MBOX-REPLY.                   <V62P07>
                                                                        <029>
           MOVE ' '         TO WSSP-SEC-ACTN (WSSP-PROGRAM-PTR).
      **** MOVE SPACES                 TO S2465-DATA-AREA.      <CL002> <GRP2.0>
           INITIALIZE                  S2465-DATA-AREA.                 <GAPPH1>
      *
           MOVE SPACES                 TO ITEM-PARAMS.                  <GRP2.0>
           MOVE T3711                  TO ITEM-ITEMTABL.                <GRP2.0>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <GRP2.0>
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.                 <GRP2.0>
           MOVE WSSP-COMPANY           TO ITEM-ITEMITEM.                <GRP2.0>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <GRP2.0>
           MOVE READR                  TO ITEM-FUNCTION.                <GRP2.0>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <GRP2.0>
           IF ITEM-STATUZ              NOT = O-K                        <GRP2.0>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <GRP2.0>
               PERFORM 600-FATAL-ERROR                                  <GRP2.0>
           END-IF.                                                      <GRP2.0>
           MOVE ITEM-GENAREA        TO T3711-T3711-REC.                 <GRP2.0>
                                                                        <MLS001>
      *--- Read TR386 table to get screen literals                      <MLS001>
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
           IF  ITEM-STATUZ          NOT = O-K                           <MLS001>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <MLS001>
               PERFORM 600-FATAL-ERROR.                                 <MLS001>
                                                                        <MLS001>
           MOVE ITEM-GENAREA           TO TR386-TR386-REC.              <MLS001>

           MOVE SPACES                 TO ITEM-PARAMS.                  <FSA566>
           MOVE TR393                  TO ITEM-ITEMTABL.                <FSA566>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <FSA566>
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.                 <FSA566>
           MOVE WSSP-FSUCO             TO ITEM-ITEMITEM.                <FSA566>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <FSA566>
           MOVE READR                  TO ITEM-FUNCTION.                <FSA566>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <FSA566>
           IF ITEM-STATUZ              NOT = O-K AND MRNF               <FSA566>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <FSA566>
               PERFORM 600-FATAL-ERROR                                  <FSA566>
           END-IF.                                                      <FSA566>
           IF ITEM-STATUZ               = O-K                           <FSA566>
              MOVE ITEM-GENAREA        TO TR393-TR393-REC               <FSA566>
           ELSE                                                         <FSA566>
              MOVE SPACES              TO TR393-TR393-REC               <FSA566>
           END-IF.                                                      <FSA566>
                                                                        <FSA566>
      *                                                                 <GRP2.0>
           MOVE SPACES                 TO WSAA-SURNAME                  <FA1971>
                                          WSAA-GIVNAME                  <FA1971>
                                          WSAA-CLNTNAME.                <FA1971>

           IF WSSP-FLAG NOT = 'A' GO TO 1200-MODIFY.
           IF WSSP-SBMACTION            = 'J'                           <V64F04>
              MOVE 'M'                 TO WSSP-FLAG                     <V64F04>
              GO TO 1200-MODIFY                                         <V64F04>
           END-IF.                                                      <V64F04>
           MOVE SPACES                 TO CLTS-DATA-AREA.
      *                                                                 <025>
           MOVE VRCM-MAX-DATE          TO S2465-START-DATE.             <025>
           MOVE SPACES                 TO DTC1-DATCON1-REC.             <025>
           MOVE TDAY                   TO DTC1-FUNCTION.                <025>
           CALL 'DATCON1' USING        DTC1-DATCON1-REC.                <025>
           MOVE DTC1-INT-DATE          TO S2465-START-DATE.             <025>
      *                                                                 <025>
           MOVE WSSP-CLNTKEY           TO WSAA-CLTSKEY.
           MOVE WSSP-LANGUAGE          TO S2465-LANGUAGE.               <025>
           MOVE WSKY-CLTS-CLNTNUM      OF WSAA-CLTSKEY
                                       TO S2465-CLNTNUM.
           MOVE WSSP-BRANCH            TO S2465-SERVBRH.
      **** MOVE 'Y'                    TO S2465-MAILING.                <031>
           MOVE SPACE                  TO S2465-MAILING.                <031>
      **** MOVE 'Y'                    TO S2465-DIRMAIL.                <031>
           MOVE SPACE                  TO S2465-DIRMAIL.                <031>
      ****--------------------------------------------------------------<032>
      ****  Address type default not appropriate in all languages.      <032>
      ****                                                              <032>
      **** MOVE 'R'                    TO S2465-ADDRTYPE.               <032>
      ****--------------------------------------------------------------<032>
      **** MOVE 'N'                    TO S2465-VIP.                    <031>
           MOVE SPACE                  TO S2465-VIP.                    <031>
           MOVE VRCM-MAX-DATE          TO S2465-CLTDOBX.
           MOVE VRCM-MAX-DATE          TO S2465-CLTDODX.
           MOVE WSAA-PROG              TO SYSR-SUBRNAME.
      ***                                                               <GRP2.0>
      ***  MOVE SPACES                 TO ITEM-PARAMS.                  <GRP2.0>
      ***  MOVE T3711                  TO ITEM-ITEMTABL.                <GRP2.0>
      ***  MOVE 'IT'                   TO ITEM-ITEMPFX.                 <GRP2.0>
      ***  MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.                 <GRP2.0>
      ***  MOVE WSSP-COMPANY           TO ITEM-ITEMITEM.                <GRP2.0>
      ***  MOVE ITEMREC                TO ITEM-FORMAT.                  <GRP2.0>
      ***  MOVE READR                  TO ITEM-FUNCTION.                <GRP2.0>
      ***  CALL 'ITEMIO'               USING ITEM-PARAMS.               <GRP2.0>
      ***  IF ITEM-STATUZ              NOT = O-K                        <GRP2.0>
      ***      MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <GRP2.0>
      ***      PERFORM 600-FATAL-ERROR                                  <GRP2.0>
      ***  END-IF.                                                      <GRP2.0>
      ***  MOVE ITEM-GENAREA        TO T3711-T3711-REC.                 <GRP2.0>
      ***  MOVE T3711-IDNUMTXT      TO S2465-IDNUMTXT.                  <GRP2.0>
      ***  MOVE T3711-CTRYCODE      TO S2465-CTRYCODE.                  <GRP2.0>
      *
                                                                        <FSA566>
           IF TR393-SUBRNAME         = SPACES                           <FSA566>
              GO TO 1100-CHECK-HEADER                                   <FSA566>
           END-IF.                                                      <FSA566>
           INITIALIZE                CLSC-CLSCVAL-REC.                  <FSA566>
           MOVE 'DFLT'               TO CLSC-FUNCTION.                  <FSA566>
           MOVE 'P'                  TO CLSC-CLTTYPE.                   <FSA566>
           MOVE TR393-TR393-REC      TO CLSC-TR393-REC.                 <FSA566>
           MOVE S2465-DATA-AREA      TO CLSC-SCREEN-DATA.               <FSA566>
           CALL TR393-SUBRNAME    USING CLSC-CLSCVAL-REC.               <FSA566>
           MOVE CLSC-SCREEN-DATA     TO S2465-DATA-AREA.                <FSA566>
      **                                                                <V5L001>
      ** S2465-LANGUAGE defaulted from WSSP-LANGUAGE but was            <V5L001>
      ** overwritten by subroutine (TR393-SUBRNAME).                    <V5L001>
           IF S2465-LANGUAGE         =  SPACES                          <V5L001>
              MOVE WSSP-LANGUAGE     TO S2465-LANGUAGE                  <V5L001>
           END-IF.                                                      <V5L001>
                                                                        <FSA566>
       1100-CHECK-HEADER.
           MOVE WSKY-CLTS-CLNTPFX OF WSAA-CLTSKEY
                                       TO CLTS-CLNTPFX.
           MOVE WSKY-CLTS-CLNTCOY OF WSAA-CLTSKEY
                                       TO CLTS-CLNTCOY.
           MOVE WSKY-CLTS-CLNTNUM OF WSAA-CLTSKEY
                                       TO CLTS-CLNTNUM.
           MOVE READR                  TO CLTS-FUNCTION.
           MOVE CLTSREC                TO CLTS-FORMAT.
           CALL 'CLTSIO'                 USING CLTS-PARAMS.
           IF CLTS-STATUZ              NOT = MRNF
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
           MOVE 'C'                    TO CLTS-CLTIND
                                          WSAA-CLTIND.
           MOVE 'N'                    TO S2465-ZDOCTIND                <FUPLET>
                                          WSAA-DOCTIND-STORE.           <FUPLET>
      **** MOVE CLTS-MARRYD            TO S2465-MARRYD.                 <GAPPH2>
      **** MOVE CLTS-CTRYCODE          TO S2465-NATLTY.                 <GAPPH2>
           GO TO 1850-ALMOST-EXIT.
      *
       1200-MODIFY.
      *
      **** MOVE SPACES                 TO S2465-DATA-AREA.              <GAPPH1>
           INITIALIZE                  S2465-DATA-AREA.                 <GAPPH1>
           MOVE WSSP-CLNTKEY           TO WSAA-CLTSKEY.
           MOVE WSKY-CLTS-CLNTNUM      OF WSAA-CLTSKEY
                                       TO S2465-CLNTNUM.
           MOVE WSAA-PROG              TO SYSR-SUBRNAME.
      *                                                                 <DA002>
      *    Get today's date
      *
       1225-CHECK-HEADER.
           MOVE WSSP-CLNTKEY           TO CLTS-DATA-KEY.
           MOVE READR                  TO CLTS-FUNCTION.
           MOVE CLTSREC                TO CLTS-FORMAT.
           CALL 'CLTSIO'                 USING CLTS-PARAMS.
           IF CLTS-STATUZ              NOT = O-K
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
      **** MOVE CLTS-SURNAME           TO S2465-SURNAME.                <PSE30>
      **** MOVE CLTS-GIVNAME           TO S2465-GIVNAME.                <PSE30>
           MOVE CLTS-LSURNAME          TO S2465-LSURNAME.               <PSE30>
           MOVE CLTS-LGIVNAME          TO S2465-LGIVNAME.               <PSE30>
      **** MOVE CLTS-SALUTL            TO S2465-SALUT.          <CAS2.0><WLI.P2>
           MOVE CLTS-SALUTL            TO S2465-SALUTL.                 <CAS2.0>
           MOVE CLTS-MARRYD            TO S2465-MARRYD.
           MOVE CLTS-BIRTHP            TO S2465-BIRTHP.
      **** MOVE CLTS-MIDDL01           TO S2465-MIDDL-01.               <PSE30>
      **** MOVE CLTS-MIDDL02           TO S2465-MIDDL-02.               <PSE30>
           MOVE CLTS-CLTSEX            TO S2465-CLTSEX.
           MOVE CLTS-CLTADDR01         TO S2465-CLTADDR-01.
           MOVE CLTS-CLTADDR02         TO S2465-CLTADDR-02.
           MOVE CLTS-CLTADDR03         TO S2465-CLTADDR-03.
           MOVE CLTS-CLTADDR04         TO S2465-CLTADDR-04.
           MOVE CLTS-CLTADDR05         TO S2465-CLTADDR-05.             <GAPPH1>
                                                                        <GAPPH2>
      **** MOVE CLTS-CLTPCODE          TO S2465-CLTPCODE.               <GAPPH1>
           IF CLTS-CLTPCODE            NOT = SPACES                     <GAPPH1>
               MOVE CLTS-CLTPCODE      TO WSAA-LOCCODE                  <GAPPH1>
               MOVE WSAA-CITY          TO S2465-ZPTCITY                 <GAPPH1>
               MOVE WSAA-DIST          TO S2465-ZPTDIST                 <GAPPH1>
               MOVE WSAA-WARD          TO S2465-ZPTWARD                 <GAPPH1>
           END-IF.                                                      <GAPPH1>
           MOVE CLTS-CTRYCODE          TO S2465-CTRYCODE.
           MOVE CLTS-CLTIND            TO WSAA-CLTIND.
           MOVE CLTS-NATLTY            TO S2465-NATLTY.
           MOVE CLTS-NATLTY            TO WSAA-NATLTY.                  <V62P07>
      *
           MOVE CLTS-MAILING           TO S2465-MAILING.
           MOVE CLTS-DIRMAIL           TO S2465-DIRMAIL.
           MOVE CLTS-ADDRTYPE          TO S2465-ADDRTYPE.
           MOVE CLTS-CLTPHONE01        TO S2465-CLTPHONE-01.
           MOVE CLTS-CLTPHONE02        TO S2465-CLTPHONE-02.
           MOVE CLTS-SERVBRH           TO S2465-SERVBRH.
           MOVE CLTS-VIP               TO S2465-VIP.
           MOVE CLTS-SECUITYNO         TO S2465-SECUITYNO.
           MOVE CLTS-SECUITYNO         TO WSAA-SECUITYNO.               <V62P07>
           MOVE CLTS-START-DATE        TO S2465-START-DATE.             <025>
      **** MOVE CLTS-ETHORIG           TO S2465-ETHORIG.        <GRP2.0><025>
           MOVE CLTS-ETHORIG           TO S2465-NMFMT.                  <GRP2.0>
           MOVE CLTS-LANGUAGE          TO S2465-LANGUAGE.               <025>
           MOVE CLTS-TAXFLAG           TO S2465-TAXFLAG.                <V63P31>
      *****MOVE CLTS-PAYROLLNO         TO S2465-PAYROLLNO.
           MOVE S2465-LGIVNAME         TO WSAA-GIVNAME.                 <FA1971>
           MOVE S2465-LSURNAME         TO WSAA-SURNAME.                 <FA1971>
           MOVE CLTS-CLTSTAT           TO S2465-CLTSTAT.                <V64F04>
           PERFORM 1300-READ-SCLTPF.                                    <GAPPH2>
                                                                        <DA002>
           MOVE S2465-SECUITYNO        TO WSAA-FIR-SECUITYNO.           <DA002>
                                                                        <DA002>
      *
           MOVE CLTS-OCCPCODE          TO S2465-OCCPCODE.
           IF S2465-OCCPCODE           = SPACES
               GO TO 1250-CONT.
      *
       1250-CONT.
      *
           MOVE CLTS-STATCODE          TO S2465-STATCODE.
           IF S2465-STATCODE           = SPACES
               GO TO 1275-CONT.
      *
      *   Get Category Code Description
      *
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.
           MOVE T3628                  TO ITEM-ITEMTABL.
           MOVE S2465-STATCODE         TO ITEM-ITEMITEM.
           PERFORM A1000-READ-ITEM.
           IF ITEM-STATUZ              = MRNF
               MOVE '??????????'       TO S2465-STATDSC
               GO TO 1275-CONT.
           PERFORM A2000-GETDESC.
           MOVE DESC-SHORTDESC         TO S2465-STATDSC.
      *
       1275-CONT.
      *
           MOVE CLTS-SOE               TO S2465-SOE.
           MOVE CLTS-DOCNO             TO S2465-DOCNO.
           IF CLTS-CLTDOB              = ZEROES
               MOVE VRCM-MAX-DATE      TO S2465-CLTDOBX
           ELSE
               MOVE CLTS-CLTDOB        TO S2465-CLTDOBX.
           IF CLTS-CLTDOD              = ZEROES
               MOVE VRCM-MAX-DATE      TO S2465-CLTDODX
           ELSE
               MOVE CLTS-CLTDOD        TO S2465-CLTDODX.
                                                                        <FUPLET>
           MOVE SPACES                 TO CLRRWIN-PARAMS.               <FUPLET>
           MOVE WSSP-COMPANY           TO CLRRWIN-FORECOY.              <FUPLET>
           MOVE S2465-CLNTNUM          TO CLRRWIN-CLNTNUM.              <FUPLET>
           MOVE CLRF-DOCTOR            TO CLRRWIN-CLRRROLE.             <FUPLET>
           MOVE BEGN                   TO CLRRWIN-FUNCTION.             <FUPLET>
           PERFORM UNTIL CLRRWIN-STATUZ = ENDP                          <FUPLET>
               CALL 'CLRRWINIO'        USING CLRRWIN-PARAMS             <FUPLET>
               IF  CLRRWIN-STATUZ      NOT = O-K                        <FUPLET>
               AND CLRRWIN-STATUZ      NOT = ENDP                       <FUPLET>
                   MOVE CLRRWIN-PARAMS TO SYSR-PARAMS                   <FUPLET>
                   PERFORM 600-FATAL-ERROR                              <FUPLET>
               END-IF                                                   <FUPLET>
               IF CLRRWIN-FORECOY      NOT = WSSP-COMPANY               <FUPLET>
               OR CLRRWIN-CLNTNUM      NOT = S2465-CLNTNUM              <FUPLET>
               OR CLRRWIN-CLRRROLE     NOT = CLRF-DOCTOR                <FUPLET>
               OR CLRRWIN-STATUZ       = ENDP                           <FUPLET>
                   MOVE ENDP           TO CLRRWIN-STATUZ                <FUPLET>
                   MOVE 'N'            TO S2465-ZDOCTIND                <FUPLET>
                                          WSAA-DOCTIND-STORE            <FUPLET>
               ELSE                                                     <FUPLET>
                   IF CLRRWIN-USED-TO-BE = SPACE                        <FUPLET>
                       MOVE ENDP       TO CLRRWIN-STATUZ                <FUPLET>
                       MOVE 'Y'        TO S2465-ZDOCTIND                <FUPLET>
                                          WSAA-DOCTIND-STORE            <FUPLET>
                   ELSE                                                 <FUPLET>
                       MOVE NEXTR      TO CLRRWIN-FUNCTION              <FUPLET>
                   END-IF                                               <FUPLET>
               END-IF                                                   <FUPLET>
           END-PERFORM.                                                 <FUPLET>
      *                                                                 <CN005>
       1400-CLIENT-EXTRA-FIELDS.                                        <CN005>
      *                                                                 <CN005>
           PERFORM A4000-CHECK-CLEX.                                    <CN005>
           MOVE 'N'                    TO WSAA-CLEX-EXISTS.             <CN005>
           IF CLEX-STATUZ = O-K                                         <CN005>
              MOVE 'Y'                 TO WSAA-CLEX-EXISTS              <CN005>
           END-IF.                                                      <CN005>
           IF WSAA-CLEX-EXISTS          = 'Y'                           <V72F01>
              MOVE CLEX-RMBLPHONE      TO S2465-RMBLPHONE               <V72F01>
              MOVE CLEX-RINTERNET      TO S2465-RINTERNET               <V72F01>
              MOVE CLEX-IDDATE         TO S2465-IDDATE                  <GAPPH1>
              MOVE CLEX-IDPLACE        TO S2465-IDPLACE                 <GAPPH1>
           ELSE                                                         <V72F01>
              MOVE SPACES              TO S2465-RMBLPHONE               <V72F01>
                                          S2465-RINTERNET               <V72F01>
              MOVE VRCM-MAX-DATE       TO S2465-IDDATE                  <GAPPH1>
           END-IF.                                                      <V72F01>
      *                                                                 <CLM14>
           PERFORM B4000-CHECK-ZCLE.                                    <CLM14>
           IF ZCLE-STATUZ = O-K                                         <CLM14>
              MOVE ZCLE-IDPLACEXT      TO S2465-IDPLACEXT               <CLM14>
           END-IF.                                                      <CLM14>
      *                                                                 <FA4547>
       1500-ADDITIONAL-DETAILS.                                         <FA4547>
      *                                                                 <FA4547>
           PERFORM A7000-CHECK-SOIN.                                    <FA4547>
           PERFORM A8000-CHECK-RACR.                                    <FA4547>
           PERFORM A9000-CHECK-BNKRPT.                                  <FA4547>
           PERFORM A10000-CHECK-CLPR.                                   <FA4547>
      *
       1850-ALMOST-EXIT.
      *
      *  Field protection is handled by four indicators. These cover
      *  four areas of the screen:
      *
      *      40 - all non-name and address details
      *      41 - the name
      *      42 - the address
      *      43 - the screen navigation check boxes
      *
           IF WSSP-FLAG = 'D'
               MOVE 'Y'                TO S2465-SERVBRH-OUT (PR)
      ****                                S2465-SURNAME-OUT (PR)        <PSE30>
                                          S2465-LSURNAME-OUT (PR)       <PSE30>
                                          S2465-CLTADDR01-OUT (PR)
                                          S2465-REXTRFLD-OUT (PR)       <CN005>
                                          S2465-BRUPIND-OUT (PR)        <V71L02>
                                          S2465-CLPRFIND-OUT (PR)       <V74F03>
                                          S2465-CLTSTAT-OUT (PR)        <FA4629>
                                          S2465-CLTDODX-OUT (PR)        <FA4629>
                                          S2465-UKPENSIND-OUT (PR).
           IF WSSP-FLAG = 'I'
               MOVE 'Y'                TO S2465-SERVBRH-OUT (PR)
      ****                                S2465-SURNAME-OUT (PR)        <PSE30>
                                          S2465-LSURNAME-OUT (PR)       <PSE30>
                                          S2465-CANFLAG-OUT (PR)        <GAPPH2>
                                          S2465-CLTADDR01-OUT (PR).
      *
           IF CLTS-CLTIND = 'D'
               MOVE 'Y'                TO S2465-SERVBRH-OUT (PR)
      ****                                S2465-SURNAME-OUT (PR)        <PSE30>
                                          S2465-LSURNAME-OUT (PR)       <PSE30>
                                          S2465-BRUPIND-OUT (PR)        <V71L02>
                                          S2465-UKPENSIND-OUT (PR)
                                          S2465-RACRIND-OUT (PR)        <V71L02>
                                          S2465-REXTRFLD-OUT (PR)       <V71L02>
                                          S2465-CLPRFIND-OUT (PR)       <V74F03>
           ELSE
           IF CLTS-CLTIND = 'L'
               MOVE 'Y'                TO S2465-SERVBRH-OUT (PR)
                                          S2465-CLTADDR01-OUT (PR)
                                          S2465-BRUPIND-OUT (PR)        <V71L02>
                                          S2465-UKPENSIND-OUT (PR)
           ELSE
           IF SCRN-POSITION-CURSOR = SPACES
            AND WSSP-FLAG NOT = 'I' AND 'D'
      ****     MOVE 'SURNAME'          TO SCRN-POSITION-CURSOR.         <PSE30>
      ****     MOVE 'LSURNAME'         TO SCRN-POSITION-CURSOR. <V72F01><PSE30>
               MOVE 'SALUTL'           TO SCRN-POSITION-CURSOR.         <V72F01>
      *
      *  The 'non-display' of a field is handled by four indicators
      *  these cover the same four areas of the screen as protect:
      *
      *      44 - all non-name and address details
      *      45 - the name
      *      46 - the address
      *      47 - the screen navigation check boxes
      *
      **** IF CLTS-CLTIND = 'D'                                         <025>
      ****     MOVE 'Y'                TO S2465-SERVBRH-OUT (ND)        <025>
      ****                                S2465-UKPENSIND-OUT (ND)      <025>
      **** ELSE                                                         <025>
      ****     IF CLTS-CLTIND = 'L'                                     <025>
      ****         MOVE 'Y'            TO S2465-SERVBRH-OUT (ND)        <025>
      ****                                S2465-CLTADDR01-OUT (ND)      <025>
      ****                                S2465-UKPENSIND-OUT (ND)      <025>
      ****     END-IF                                                   <025>
      **** END-IF.                                                      <025>
      *
           IF WSSP-FLAG                 = 'I'                           <V64F04>
              MOVE 'Y'                 TO S2465-CLTSTAT-OUT(PR)         <V64F04>
                                          S2465-CLTDODX-OUT(PR)         <V64F04>
      ***  ELSE                                                         <V64F04>
      ***     MOVE 'N'                 TO S2465-CLTSTAT-OUT(PR)         <V64F04>
      ***                                 S2465-CLTDODX-OUT(PR)         <V64F04>
           END-IF.                                                      <V64F04>
                                                                        <V64F04>
           IF WSSP-SBMACTION           = 'J'                            <V64F04>
              MOVE 'M'                TO WSSP-FLAG                      <V64F04>
              MOVE 'Y'                TO S2465-SERVBRH-OUT (PR)         <V64F04>
                                         S2465-LSURNAME-OUT (PR)        <V64F04>
                                         S2465-CLTADDR01-OUT (PR)       <V64F04>
                                         S2465-REXTRFLD-OUT (PR)        <V64F04>
                                         S2465-BRUPIND-OUT (PR)         <V71L02>
                                         S2465-UKPENSIND-OUT (PR)       <V64F04>
                                         S2465-RACRIND-OUT (PR)         <V64F04>
                                         S2465-CLPRFIND-OUT (PR)        <V74F03>
                                         S2465-SALUTL-OUT (PR)          <FA4629>
              MOVE 'N'                TO S2465-CLTSTAT-OUT (PR)         <V64F04>
                                         S2465-CLTDODX-OUT (PR)         <V64F04>
           END-IF.                                                      <V64F04>
                                                                        <S9503>>
      * Set up a working storage variable for the CLTS countrycode      <A05912>
      * record which can be compared against the screen variable        <A05912>
      * and then updated.                                               <A05912>
      *                                                                 <A05912>
           MOVE CLTS-CTRYCODE          TO WSAA-CLTS-CTRYCODE.           <A05912>
      *                                                                 <A05912>
      *                                                                 <V4F001>
       1860-NAME-FORMAT.                                                <V4F001>
      * Default Name Format to '1' upon adding new personal client.     <V4F001>
                                                                        <V4F001>
           IF  S2465-NMFMT              = SPACES                        <V4F001>
           AND WSSP-FLAG                = 'A'                           <V4F001>
               MOVE TR393-NMFMT         TO S2465-NMFMT.                 <FA1162>
    ****       MOVE '1'                 TO S2465-NMFMT.                 <FA1162>
      *                                                                 <PHFX02>
       1900-EXIT.
            EXIT.
      *                                                                 <GAPPH2>
      /                                                                 <GAPPH2>
       1300-READ-SCLTPF SECTION.                                        <GAPPH2>
      **************************                                        <GAPPH2>
      *                                                                 <GAPPH2>
       1310-START.                                                      <GAPPH2>
      *                                                                 <GAPPH2>
           MOVE SPACES                 TO WSAA-STAFFLAG-SAV.            <GAPPH2>
           MOVE SPACES                 TO SCLTENQ-PARAMS.               <GAPPH2>
           MOVE WSSP-CLNTKEY           TO SCLTENQ-DATA-KEY.             <GAPPH2>
           MOVE SCLTENQREC             TO SCLTENQ-FORMAT.               <GAPPH2>
           MOVE READR                  TO SCLTENQ-FUNCTION.             <GAPPH2>
                                                                        <GAPPH2>
           CALL 'SCLTENQIO'         USING SCLTENQ-PARAMS.               <GAPPH2>
                                                                        <GAPPH2>
           IF SCLTENQ-STATUZ        NOT = O-K AND MRNF                  <GAPPH2>
               MOVE SCLTENQ-PARAMS     TO SYSR-PARAMS                   <GAPPH2>
               PERFORM 600-FATAL-ERROR                                  <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           IF SCLTENQ-STATUZ            = O-K                           <GAPPH2>
              IF SCLTENQ-RSTAFLAG   NOT = SPACES                        <GAPPH2>
                 IF SCLTENQ-PRTFLG      = 'Y'                           <GAPPH2>
                      MOVE 'N'         TO S2465-CANFLAG                 <GAPPH2>
                 ELSE                                                   <GAPPH2>
                      MOVE 'Y'         TO S2465-CANFLAG                 <GAPPH2>
                 END-IF                                                 <GAPPH2>
              ELSE                                                      <GAPPH2>
                 MOVE SPACES           TO S2465-CANFLAG                 <GAPPH2>
              END-IF                                                    <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           MOVE S2465-CANFLAG          TO WSAA-STAFFLAG-SAV.            <GAPPH2>
      *                                                                 <GAPPH2>
       1390-EXIT.                                                       <GAPPH2>
           EXIT.                                                        <GAPPH2>
      *                                                                 <GAPPH2>
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
           MOVE O-K                    TO WSSP-EDTERROR.                <S9503>
           MOVE 'E'                 TO S2465-LANGUAGE.                  <NB006>
           MOVE 'Y'                 TO S2465-LANGUAGE-OUT(PR).          <NB006>
                                                                        <GAPPH2>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <S9503>
              MOVE 3000                TO WSSP-SECTIONNO                <S9503>
           GO TO PRE-EXIT.                                              <S9503>
                                                                        <GAPPH2>
           IF WSSP-FLAG = 'A'                                           <GAPPH2>
           AND (S2465-IDDATE           = ZEROES OR                      <GAPPH2>
                S2465-IDDATE           = VRCM-MAX-DATE)                 <GAPPH2>
               MOVE VRCM-MAX-DATE          TO S2465-IDDATE              <GAPPH2>
           END-IF.                                                      <GAPPH2>
      *                                                                 <S9503>
   ****                                                         <FA4547><S9503>
   ****    Check salary history indicator                       <FA4547><S9503>
   ****                                                         <FA4547><S9503>
   ****    MOVE WSSP-FSUCO             TO SOIN-CLNTCOY.         <FA4547><S9503>
   ****    MOVE S2465-CLNTNUM          TO SOIN-CLNTNUM.         <FA4547><S9503>
   ****    MOVE ZERO                   TO SOIN-INCOME-SEQ-NO.   <FA4547><S9503>
   ****    MOVE BEGN                   TO SOIN-FUNCTION.        <FA4547><S9503>
   ****    MOVE SOINREC                TO SOIN-FORMAT.          <FA4547><S9503>
   ****    CALL 'SOINIO'               USING SOIN-PARAMS.       <FA4547><S9503>
   ****    IF SOIN-STATUZ              NOT = O-K AND ENDP       <FA4547><S9503>
   ****        MOVE SOIN-PARAMS        TO SYSR-PARAMS           <FA4547><S9503>
   ****        PERFORM 600-FATAL-ERROR.                         <FA4547><S9503>
   ****                                                         <FA4547><S9503>
   ****    IF SOIN-STATUZ              = ENDP                   <FA4547><S9503>
   ****     OR (SOIN-CLNTCOY           NOT = WSSP-FSUCO         <FA4547><S9503>
   ****          OR SOIN-CLNTNUM       NOT = S2465-CLNTNUM)     <FA4547><S9503>
   ****        MOVE SPACE              TO S2465-UK-PENSION-IND  <FA4547><S9503>
   ****    ELSE                                                 <FA4547><S9503>
   ****        MOVE '+'                TO S2465-UK-PENSION-IND. <FA4547><S9503>
                                                                        <S9503>
           IF WSSP-FLAG = 'I'                                           <S9503>
            AND S2465-UK-PENSION-IND   = SPACES                         <S9503>
               MOVE 'Y'                TO S2465-UKPENSIND-OUT (PR)      <S9503>
           END-IF.                                                      <S9503>
                                                                        <V64F04>
   ****    Check bankruptcy indicator                           <FA4547><V71L02>
   ****                                                         <FA4547><V71L02>
   ****    INITIALIZE                  BRUP-PARAMS.             <FA4547><V71L02>
   ****    MOVE CLTS-CLNTCOY           TO BRUP-CLNTCOY.         <FA4547><V71L02>
   ****    MOVE CLTS-CLNTNUM           TO BRUP-CLNTNUM.         <FA4547><V71L02>
   ****    MOVE ZERO                   TO BRUP-BRUPDTE.         <FA4547><V71L02>
   ****    MOVE BEGN                   TO BRUP-FUNCTION.        <FA4547><V71L02>
   ****    MOVE BRUPREC                TO BRUP-FORMAT.          <FA4547><V71L02>
   ****                                                         <FA4547><V71L02>
   ****    CALL 'BRUPIO'               USING BRUP-PARAMS.       <FA4547><V71L02>
   ****                                                         <FA4547><V71L02>
   ****    IF BRUP-STATUZ              NOT = O-K                <FA4547><V71L02>
   ****    AND                         ENDP                     <FA4547><V71L02>
   ****        MOVE BRUP-STATUZ        TO SYSR-STATUZ           <FA4547><V71L02>
   ****        MOVE BRUP-PARAMS        TO SYSR-PARAMS           <FA4547><V71L02>
   ****        PERFORM 600-FATAL-ERROR.                         <FA4547><V71L02>
   ****                                                         <FA4547><V71L02>
   ****    IF BRUP-STATUZ              = ENDP                   <FA4547><V71L02>
   ****     OR (BRUP-CLNTCOY           NOT = CLTS-CLNTCOY       <FA4547><V71L02>
   ****          OR BRUP-CLNTNUM       NOT = CLTS-CLNTNUM)      <FA4547><V71L02>
   ****        MOVE SPACE              TO S2465-BRUPIND         <FA4547><V71L02>
   ****    ELSE                                                 <FA4547><V71L02>
   ****        MOVE '+'                TO S2465-BRUPIND.        <FA4547><V71L02>
                                                                        <V71L02>
           IF WSSP-FLAG = 'I'                                           <V71L02>
           AND S2465-BRUPIND         = SPACES                           <V71L02>
              MOVE 'Y'                TO S2465-BRUPIND-OUT (PR)         <V71L02>
           END-IF.                                                      <V71L02>
                                                                        <V71L02>
      * Check Client Profiling Indicator                                <V74F03>
      **   INITIALIZE                     CLPR-PARAMS.          <FA4547><V74F03>
      **   MOVE CLTS-CLNTPFX           TO CLPR-CLNTPFX.         <FA4547><V74F03>
      **   MOVE CLTS-CLNTCOY           TO CLPR-CLNTCOY.         <FA4547><V74F03>
      **   MOVE CLTS-CLNTNUM           TO CLPR-CLNTNUM.         <FA4547><V74F03>
      **   MOVE READR                  TO CLPR-FUNCTION.        <FA4547><V74F03>
      **   MOVE CLPRREC                TO CLPR-FORMAT.          <FA4547><V74F03>
                                                                        <V74F03>
      **   CALL 'CLPRIO'            USING CLPR-PARAMS.          <FA4547><V74F03>
                                                                        <V74F03>
      **   IF CLPR-STATUZ           NOT = O-K AND MRNF          <FA4547><V74F03>
      **       MOVE CLPR-STATUZ        TO SYSR-STATUZ           <FA4547><V74F03>
      **       MOVE CLPR-PARAMS        TO SYSR-PARAMS           <FA4547><V74F03>
      **       PERFORM 600-FATAL-ERROR                          <FA4547><V74F03>
      **   END-IF.                                              <FA4547><V74F03>
                                                                        <V74F03>
      **   IF CLPR-STATUZ               = MRNF                  <FA4547><V74F03>
      **      MOVE SPACE               TO S2465-CLPRFIND        <FA4547><V74F03>
      **   ELSE                                                 <FA4547><V74F03>
      **      MOVE '+'                 TO S2465-CLPRFIND        <FA4547><V74F03>
      **   END-IF.                                              <FA4547><V74F03>
                                                                        <V74F03>
           IF WSSP-FLAG = 'I'                                           <V74F03>
           AND S2465-CLPRFIND          = SPACES                         <V74F03>
              MOVE 'Y'                 TO S2465-CLPRFIND-OUT (PR)       <V74F03>
           END-IF.                                                      <V74F03>
                                                                        <V74F03>
      **** IF WSSP-SBMACTION            = 'C'                           <V64F04>
      ****    MOVE 'Y'                 TO S2465-CLTSTAT-OUT(PR)         <V64F04>
      **** END-IF.                                                      <V64F04>
                                                                        <V64F04>
           IF WSSP-SBMACTION            = 'A'                           <V64F04>
             OR WSSP-SBMACTION          = '8'                           <V64F04>
              MOVE 'AC'                TO S2465-CLTSTAT                 <V64F04>
              MOVE 'Y'                 TO S2465-CLTSTAT-OUT(PR)         <V64F04>
           END-IF.                                                      <V64F04>
                                                                        <FA3869>
           IF  WSSP-SBMACTION           = 'C'                           <FA3869>
              MOVE 'Y'                 TO S2465-CLTSTAT-OUT(PR)         <FA3869>
              IF S2465-CLTSTAT      NOT = 'DN'                          <FA3869>
                 MOVE 'Y'              TO S2465-CLTDODX-OUT(PR)         <FA3869>
              END-IF                                                    <FA3869>
           END-IF.                                                      <FA3869>
                                                                        <S9503>>
   ****    MOVE SPACES                 TO RACR-PARAMS.          <FA4547><S9503>>
   ****    MOVE SPACES                 TO S2465-RACRIND.        <FA4547><S9503>>
   ****                                                         <FA4547><S9503>>
   ****    MOVE WSSP-FSUCO             TO RACR-CLNTCOY.         <FA4547><S9503>>
   ****    MOVE CLTS-CLNTPFX           TO RACR-CLNTPFX.         <FA4547><S9503>>
   ****    MOVE CLTS-CLNTNUM           TO RACR-CLNTNUM.         <FA4547><S9503>>
   ****    MOVE SPACES                 TO RACR-LRKCLS.          <FA4547><S9503>>
   ****    MOVE ZEROES                 TO RACR-CURRFROM.        <FA4547><S9503>>
   ****                                                         <FA4547><S9503>>
   ****    MOVE BEGN                   TO RACR-FUNCTION.        <FA4547><S9503>>
   ****                                                         <FA4547><S9503>>
   ****    PERFORM 2200-CHECK-FOR-RACR UNTIL RACR-STATUZ = ENDP.<FA4547><S9503>>
      *                                                                 <CN005>
      *    Check extra fields indicator                                 <CN005>
      *                                                                 <CN005>
           IF WSSP-FLAG                = 'D'  OR                        <CN005>
              (WSSP-FLAG               = 'I'  AND                       <CN005>
              NOT CLEX-EXISTS)                                          <CN005>
               MOVE 'Y'                TO S2465-REXTRFLD-OUT (PR)       <CN005>
           END-IF.                                                      <CN005>
                                                                        <V72F01>
           IF WSSP-FLAG                = 'D'  OR                        <V72F01>
              (WSSP-FLAG               = 'I'  AND                       <V72F01>
               CLEX-EXISTS)                                             <V72F01>
               IF CLEX-RDIDTELNO        = SPACE AND                     <V72F01>
                  CLEX-RPAGER           = SPACE AND                     <V72F01>
                  CLEX-FAXNO            = SPACE AND                     <V72F01>
                  CLEX-RTAXIDNUM        = SPACE AND                     <V72F01>
                  CLEX-RSTAFLAG         = SPACE AND                     <V72F01>
                  CLEX-ZSPECIND         = SPACE AND                     <V72F01>
                  CLEX-OLDIDNO          = SPACE                         <V72F01>
                  MOVE 'Y'                TO S2465-REXTRFLD-OUT (PR)    <V72F01>
               END-IF                                                   <V72F01>
           END-IF.                                                      <V72F01>

           IF WSSP-FLAG                = 'D'  OR                        <PSE309>
              (WSSP-FLAG               = 'I'  AND                       <PSE309>
              NOT RACR-EXISTS)                                          <PSE309>
               MOVE 'Y'                TO S2465-RACRIND-OUT (PR)        <PSE309>
           END-IF.                                                      <PSE309>
                                                                        <V64F04>
      *                                                         <WLI.P2><S9503>
      * Read Address validation table and display Address literals      <S9503>
      *                                                                 <S9503>
      **** PERFORM 6000-READ-ADDR-VAL-TABLE.                            <S9503>>
      **** MOVE T2241-ADDRDESC (01)    TO S2465-ADDRDESC (01).          <S9503>>
      **** MOVE T2241-ADDRDESC (02)    TO S2465-ADDRDESC (02).          <S9503>>
      **** MOVE T2241-ADDRDESC (03)    TO S2465-ADDRDESC (03).          <S9503>>
      **** MOVE T2241-ADDRDESC (04)    TO S2465-ADDRDESC (04).          <S9503>>
      **** MOVE T2241-ADDRDESC (05)    TO S2465-ADDRDESC (05).          <S9503>>
      *                                                                 <S9503>>
      ***** Only access this table if this is the first loop around     <S9503>>
      ***** this section or if the countrycode or language has changed. <S9503>>
                                                                        <S9503>>
           IF FIRST-REC                                                 <S9503>>
           OR COUNTRY-CHANGED                                           <S9503>>
           OR LANG-CHANGED                                              <S9503>>
              PERFORM 6000-READ-ADDR-VAL-TABLE                          <S9503>>
              MOVE T2241-ADDRDESC (01)    TO S2465-ADDRDESC (01)        <S9503>>
              MOVE T2241-ADDRDESC (02)    TO S2465-ADDRDESC (02)        <S9503>>
              MOVE T2241-ADDRDESC (03)    TO S2465-ADDRDESC (03)        <S9503>>
              MOVE T2241-ADDRDESC (04)    TO S2465-ADDRDESC (04)        <S9503>>
              MOVE T2241-ADDRDESC (05)    TO S2465-ADDRDESC (05)        <S9503>>
              MOVE 'N'                    TO WSAA-COUNTRY               <S9503>>
              MOVE 'N'                    TO WSAA-LANG-CHANGED          <S9503>>
           END-IF.                                                      <S9503>>
      *                                                                 <S9503>
      *   Display the screen at last!                                   <S9503>
      *                                                                 <S9503>
           IF WSAA-INIT = 'Y'                                           <S9503>
               MOVE 'N'                    TO WSAA-INIT                 <S9503>
               MOVE INIT                   TO SCRN-FUNCTION             <S9503>
           ELSE                                                         <S9503>
               MOVE NORML                  TO SCRN-FUNCTION.            <S9503>
                                                                        <S9503>>
      **** MOVE T3711-IDNUMTXT         TO S2465-IDNUMTXT.        <S9503><MLS001>
      ***  MOVE TR386-PROGDESC(1)      TO S2465-IDNUMTXT.               <MLS001>
      **** MOVE TR386-PROGDESC-01      TO S2465-IDNUMTXT.       <V72F01><FA1226>
           MOVE TR386-PROGDESC-01      TO S2465-IDNOTXT.                <V72F01>
           MOVE TR386-PROGDESC-03      TO S2465-SCRTITLE.               <V63P31>
    ****   MOVE T3711-CTRYCODE         TO S2465-CTRYCODE.       <WLI.P2><S9503>>
           IF S2465-CTRYCODE           = SPACES                         <S9503>>
              MOVE T3711-CTRYCODE      TO S2465-CTRYCODE                <S9503>>
           END-IF.                                                      <S9503>>
      *                                                                 <GAPPH1>
      * Do not show field CLTPCODE:                                     <GAPPH1>
      *                                                                 <GAPPH1>
           MOVE 'Y'                    TO S2465-CLTPCODE-OUT(ND).       <GAPPH1>
           IF WSSP-FLAG                = 'I'                            <GAPPH1>
           OR WSSP-SBMACTION           = 'J'                            <GAPPH1>
               MOVE 'Y'                TO S2465-ZPTCITY-OUT(PR)         <GAPPH1>
                                          S2465-ZPTDIST-OUT(PR)         <GAPPH1>
                                          S2465-ZPTWARD-OUT(PR)         <GAPPH1>
           END-IF.                                                      <GAPPH1>
      *                                                                 <GAPPH1>
      * Protect field Name Format:                                      <GAPPH1>
      *                                                                 <GAPPH1>
           MOVE 'Y'                    TO S2465-NMFMT-OUT(PR).          <GAPPH1>
      *                                                                 <GAPPH2>
      * Save data on sreen                                              <GAPPH2>
      *                                                                 <GAPPH2>
      **** MOVE S2465-DATA-FIELDS      TO WSAA-CLNT-DATA-SAVE.  <PHFX02><GAPPH2>
           IF WSAA-FIRST-TIME           = 'Y'                           <PHFX02>
              MOVE S2465-DATA-FIELDS      TO WSAA-CLNT-DATA-SAVE        <PHFX02>
              MOVE 'N'                    TO WSAA-FIRST-TIME            <PHFX02>
           END-IF.                                                      <PHFX02>
      *                                                                 <GAPPH1>
           GO TO PRE-EXIT.                                              <S9503>
      *                                                                 <S9503>
       PRE-EXIT.                                                        <S9503>
           EXIT.                                                        <S9503>
      /                                                                 <S9503>
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2001-SCREEN-IO.
      *    CALL 'S2465IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                          S2465-DATA-AREA.                       <S9503>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.
      *                                                                 <A05912>
           IF WSSP-FLAG                = 'I'                            <A05912>
              MOVE SPACES              TO S2465-ERROR-INDICATORS        <A05912>
           END-IF.                                                      <A05912>
      *
      *   Skip screen if CF11 pressed - kill.
      *
           IF SCRN-STATUZ = KILL
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *       GO TO 2900-EXIT.                                          <S9503>
      ****    GO TO 2090-EXIT.                                  <PSE30> <S9503>
      ****    GO TO 2000-CONT.                                  FSA1115 <PSE30>
              GO TO 2090-EXIT.                                          FSA1115
      *
      **** Redisplay screen if CALC is pressed
      *
           IF SCRN-STATUZ              = 'CALC'
               PERFORM 2300-SHOW-ADDRESS-LINES                          <GAPPH1>
               MOVE 'Y'                TO WSSP-EDTERROR
           END-IF.
                                                                        <NB008>
           IF SCRN-STATUZ              = 'CALC'                         <NB008>
              MOVE S2465-RMBLPHONE     TO WSAA-MB-NUM                   <NB008>
              MOVE S2465-CLTPHONE-02   TO WSAA-OF-NUM                   <NB008>
              MOVE S2465-CLTPHONE-01   TO WSAA-HS-NUM                   <NB008>
              MOVE SPACES              TO WSAA-MB-WARNING               <NB008>
                                          WSAA-OF-WARNING               <NB008>
                                          WSAA-HS-WARNING               <NB008>
                                          WSAA-SECURI-FND               <NB008>
           END-IF.                                                      <NB008>
                                                                        <NB008>
                                                                        <025>
           IF S2465-ERROR-INDICATORS   NOT = SPACES                     <025>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <025>
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *       GO TO 2999-EXIT                                           <S9503>
              GO TO 2090-EXIT                                           <S9503>
           END-IF.                                                      <025>
      *                                                                 <PS013>
      *  Skip validation when F9.                                       <PS013>
      *                                                                 <PS013>
           IF SCRN-STATUZ              = 'DIRY'                         <PS013>
           AND WSSP-SBMACTION          = 'C'                            <PS013>
              GO TO 2090-EXIT                                           <PS013>
           END-IF.                                                      <PS013>
                                                                        <PS013>
      *                                                                 <FA2360>
      * skip validation when doing an inquiry                           <FA2360>
      *                                                                 <FA2360>
           IF  WSSP-SBMACTION    = 'D'                                  <FA2360>
               IF SCRN-STATUZ            = 'DIRY' OR 'SERV'             <FA3358>
                  MOVE G602             TO SCRN-ERROR-CODE              <FA3358>
                  MOVE 'Y'              TO WSSP-EDTERROR                <FA3358>
               END-IF                                                   <FA3358>
               GO TO 2090-EXIT.                                         <FA2360>
                                                                        <V64F04>
           IF S2465-CLTSTAT             = 'DN'                          <V64F04>
           OR S2465-CLTSTAT             = 'DC'                          <V64F04>
              IF S2465-CLTDODX             > WSAA-TODAY                 <V64F04>
                 IF  S2465-CLTDODX      < VRCM-MAX-DATE                 <V64F04>
                     MOVE F073          TO S2465-CLTDODX-ERR            <V64F04>
                 ELSE                                                   <V64F04>
                     MOVE I032          TO S2465-CLTDODX-ERR            <V64F04>
                 END-IF                                                 <V64F04>
                 GO TO 2080-CHECK-ERROR                                 <V64F04>
              END-IF                                                    <V64F04>
           END-IF.                                                      <V64F04>
                                                                        <V64F04>
           IF WSSP-SBMACTION            = 'C'                           <V64F04>
              IF S2465-CLTSTAT          = 'DN'                          <V64F04>
              OR S2465-CLTSTAT          = 'DC'                          <V64F04>
                 IF S2465-CLTDODX       = VRCM-MAX-DATE OR SPACES       <V64F04>
                    MOVE H366          TO S2465-CLTDODX-ERR             <V64F04>
                    GO TO 2080-CHECK-ERROR                              <V64F04>
                 END-IF                                                 <V64F04>
              ELSE                                                      <FA3869>
                 IF S2465-CLTDODX   NOT = VRCM-MAX-DATE AND SPACES      <FA3869>
                    MOVE E374          TO S2465-CLTDODX-ERR             <FA3869>
                    GO TO 2080-CHECK-ERROR                              <FA3869>
                 END-IF                                                 <FA3869>
              END-IF                                                    <V64F04>
           END-IF.                                                      <V64F04>
                                                                        <V64F04>
           IF WSSP-SBMACTION            = 'J'                           <V64F04>
              IF S2465-CLTSTAT          = SPACES                        <V64F04>
                 MOVE E186             TO S2465-CLTSTAT-ERR             <V64F04>
                 GO TO 2080-CHECK-ERROR                                 <V64F04>
              ELSE                                                      <V64F04>
                 MOVE SPACES           TO ITEM-DATA-AREA                <V64F04>
                 MOVE 'IT'             TO ITEM-ITEMPFX                  <V64F04>
                 MOVE WSSP-FSUCO       TO ITEM-ITEMCOY                  <V64F04>
                 MOVE T3643            TO ITEM-ITEMTABL                 <V64F04>
                 MOVE S2465-CLTSTAT    TO ITEM-ITEMITEM                 <V64F04>
                 MOVE SPACES           TO ITEM-ITEMSEQ                  <V64F04>
                 MOVE READR            TO ITEM-FUNCTION                 <V64F04>
                                                                        <V64F04>
                 CALL 'ITEMIO'      USING ITEM-PARAMS                   <V64F04>
                 IF ITEM-STATUZ     NOT = O-K AND MRNF                  <V64F04>
                    MOVE ITEM-STATUZ   TO SYSR-STATUZ                   <V64F04>
                    MOVE ITEM-PARAMS   TO SYSR-PARAMS                   <V64F04>
                    PERFORM 600-FATAL-ERROR                             <V64F04>
                 END-IF                                                 <V64F04>
                                                                        <V64F04>
                 IF ITEM-STATUZ         = MRNF                          <V64F04>
                    MOVE F991          TO  S2465-CLTSTAT-ERR            <V64F04>
                    GO TO 2090-EXIT                                     <V64F04>
                 END-IF                                                 <V64F04>
              END-IF                                                    <V64F04>
                                                                        <V64F04>
              IF S2465-CLTSTAT          = 'DN'                          <V64F04>
              OR S2465-CLTSTAT          = 'DC'                          <V64F04>
                 IF S2465-CLTDODX       = VRCM-MAX-DATE OR SPACES       <V64F04>
                    MOVE E186          TO S2465-CLTDODX-ERR             <V64F04>
                    MOVE 'N'           TO S2465-CLTDODX-OUT(PR)         <V64F04>
                    GO TO 2080-CHECK-ERROR                              <V64F04>
                 END-IF                                                 <V64F04>
              ELSE                                                      <V64F04>
                 IF S2465-CLTDODX   NOT = VRCM-MAX-DATE                 <V64F04>
                    MOVE E374          TO S2465-CLTDODX-ERR             <V64F04>
                    GO TO 2080-CHECK-ERROR                              <V64F04>
                 END-IF                                                 <V64F04>
              END-IF                                                    <V64F04>
              IF  SCRN-STATUZ          = 'DIRY'                         <FA4629>
              OR  SCRN-STATUZ          = 'SERV'                         <FA4629>
                  MOVE G602            TO SCRN-ERROR-CODE               <FA4629>
                  MOVE 'Y'             TO WSSP-EDTERROR                 <FA4629>
              END-IF                                                    <FA4629>
              GO TO 2090-EXIT                                           <V64F04>
           END-IF.                                                      <V64F04>
      *
      *    For address or alias create use subroutine to find
      *    program and perform dynamic call to the program
      *
           IF WSSP-FLAG = 'D'
                       OR 'I'
                       OR 'A'
              OR CLTS-CLTIND = 'D'
                            OR 'L'
               GO TO 2050-CONT.

      *
       2050-CONT.
      *
           IF WSSP-FLAG NOT = 'I'
                    AND NOT = 'D'
                    AND NOT = 'A'
              AND CLTS-CLTIND NOT = 'D'
                    AND NOT = 'L'
              GO TO 2125-VALIDATE.
      *
             IF SCRN-STATUZ = 'DIRY'
                           OR 'SERV'
               MOVE G602               TO SCRN-ERROR-CODE
               MOVE 'Y'                TO WSSP-EDTERROR.
      *
           IF WSSP-FLAG = 'D'
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *        GO TO 2900-EXIT.                                         <S9503>
      ****     GO TO 2090-EXIT.                                 <PSE30> <S9503>
               GO TO 2000-CONT.                                         <PSE30>
      *
       2125-VALIDATE.
      *

      * If client creation, check for any existing client with same     <FA5050>
      * Client number in the database.                                  <FA5050>
           IF WSSP-FLAG                 = 'A' AND                       <FA5050>
              WSAA-FIRST-LOOP           = 'Y'                           <FA5050>
              MOVE WSSP-CLNTKEY        TO CLTS-DATA-KEY                 <FA5050>
              MOVE READR               TO CLTS-FUNCTION                 <FA5050>
              MOVE CLTSREC             TO CLTS-FORMAT                   <FA5050>
              CALL 'CLTSIO'         USING CLTS-PARAMS                   <FA5050>
              IF CLTS-STATUZ        NOT = O-K AND MRNF                  <FA5050>
                 MOVE CLTS-PARAMS      TO SYSR-PARAMS                   <FA5050>
                 PERFORM 600-FATAL-ERROR                                <FA5050>
              END-IF                                                    <FA5050>
              IF CLTS-STATUZ            = O-K                           <FA5050>
                 MOVE E057             TO S2465-CLNTNUM-ERR             <FA5050>
                 GO TO 2000-CONT                                        <FA5050>
              END-IF                                                    <FA5050>
           END-IF.                                                      <FA5050>
           IF S2465-UK-PENSION-IND NOT = 'X' AND '+' AND ' '
              MOVE G620                TO S2465-UKPENSIND-ERR.
      *                                                                 <V71L02>
           IF S2465-BRUPIND        NOT = 'X' AND '+' AND ' '            <V71L02>
              MOVE G620                TO S2465-BRUPIND-ERR.            <V71L02>
      *                                                                 <V71L02>
           IF S2465-RACRIND        NOT = 'X' AND '+' AND ' '            <V71L02>
              MOVE G620                TO S2465-RACRIND-ERR             <V71L02>
           END-IF.                                                      <V71L02>
                                                                        <V71L02>
           IF S2465-REXTRFLD        NOT = 'X' AND '+' AND ' '           <V71L02>
              MOVE G620                TO S2465-REXTRFLD-ERR.           <CN005>
                                                                        <V74F03>
           IF S2465-CLPRFIND        NOT = 'X' AND '+' AND ' '           <V74F03>
              MOVE G620                TO S2465-CLPRFIND-ERR            <V74F03>
           END-IF.                                                      <V74F03>
                                                                        <V74F03>
      *                                                         <R96REA>
           IF WSSP-FLAG = 'I'
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *        GO TO 2900-EXIT.                                         <S9503>
      ****     GO TO 2090-EXIT.                                 <PSE30> <S9503>
               GO TO 2000-CONT.                                         <PSE30>
      *
      *    These fields are checked on the appropriate tables by the
      *    IO module using error messages set up in the field dict.
      *    CLTSEX      T3582    G979
      *    CTRYCODE    T3645    F993
      *    SERVBRH     T1692    E008
      *    OCCPCODE    T3644    F992
      *    STATCODE    T3628    F952
      *    MARRYD      T3571    Z996
      ****                                                              <032>
      *    Surname is validated by A3000-VALNAME
      *=================================================================<032>
      *    -- Modify name validation to check the character set  --     <032>
      *    -- on T3716 for the client language.                  --     <032>
      *    -- If language and country have not been set up,      --     <032>
      *    -- default to item '****'.                            --     <032>
      *=================================================================<032>
           MOVE VALN-NAME              TO VALN-FUNCTION.
      *    IF S2465-LANGUAGE           = SPACE                  <NB006> <032>
      *       MOVE '*'                 TO WSAA-LANGUAGE         <NB006> <032>
      *    ELSE                                                 <NB006> <032>
      *       MOVE S2465-LANGUAGE      TO WSAA-LANGUAGE         <NB006> <032>
      *    END-IF.                                              <NB006> <032>
           IF S2465-CTRYCODE           = SPACE                          <NB006>
              MOVE '***'               TO WSAA-CTRYCODE                 <032>
           ELSE                                                         <032>
              MOVE S2465-CTRYCODE      TO WSAA-CTRYCODE                 <032>
           END-IF.                                                      <032>
                                                                        <NB006>
      *                                                                 <A06259>
       2127-READ-T3716.                                                 <A06259>
      *                                                                 <A06259>
           MOVE SPACES                 TO ITEM-PARAMS.                  <032>
           MOVE T3716                  TO ITEM-ITEMTABL.                <032>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <032>
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.                 <032>
           MOVE WSAA-ITEM              TO ITEM-ITEMITEM.                <032>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <032>
      *****MOVE BEGN                   TO ITEM-FUNCTION.        <A06259><032>
           MOVE READR                  TO ITEM-FUNCTION.                <A06259>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <032>
      *****IF ITEM-STATUZ              NOT = O-K                <A06259><032>
           IF ITEM-STATUZ              NOT = O-K AND                    <A06259>
              ITEM-STATUZ              NOT = MRNF                       <A06259>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <032>
               PERFORM 600-FATAL-ERROR                                  <032>
           END-IF.                                                      <032>
      *                                                                 <032>
      *    Default item has not been found so error message produced    <A06259>
      *                                                                 <A06259>
           IF WSAA-ITEM                = '****' AND                     <A06259>
              ITEM-STATUZ              = MRNF                           <A06259>
              MOVE D037                TO S2465-CTRYCODE-ERR            <A06259>
              GO TO 2130-NEXT                                           <A06259>
           END-IF.                                                      <A06259>
      *                                                                 <A06259>
      *    The read failed with real language or country so replace     <A06259>
      *    with *'s and try for default records.                        <A06259>
      *                                                                 <A06259>
           IF ITEM-STATUZ              = MRNF AND                       <A06259>
              WSAA-CTRYCODE            NOT = '***'                      <A06259>
              MOVE '***'               TO WSAA-CTRYCODE                 <A06259>
              GO TO 2127-READ-T3716                                     <A06259>
           END-IF.                                                      <A06259>
      *                                                                 <A06259>
           IF ITEM-STATUZ              = MRNF AND                       <A06259>
              WSAA-LANGUAGE            NOT = '*'                        <A06259>
              MOVE '*'                 TO WSAA-LANGUAGE                 <A06259>
              GO TO 2127-READ-T3716                                     <A06259>
           END-IF.                                                      <A06259>
      *                                                                 <A06259>
      **** MOVE ITEM-ITEMITEM          TO WSAA-ITEM.            <A06259><032>
      **** IF ITEM-STATUZ              = O-K AND                <A06259><032>
      ****    WSAA-LANGUAGE            = S2465-LANGUAGE         <A06259><032>
      ****    MOVE ITEM-GENAREA        TO T3716-T3716-REC       <A06259><032>
      ****    MOVE T3716-CHARSET-01    TO WSAA-UPPER            <A06259><032>
      ****    MOVE T3716-CHARSET-02    TO WSAA-LOWER            <A06259><032>
      ****    MOVE T3716-CHARSET-03    TO WSAA-SPECIAL          <A06259><032>
      **** ELSE                                                 <A06259><032>
      ****    MOVE D037                TO S2465-LANGUAGE-ERR    <A06259><032>
      **** END-IF.                                              <A06259><032>
      *                                                                 <A06259>
           MOVE ITEM-GENAREA        TO T3716-T3716-REC.                 <A06259>
           MOVE T3716-CHARSET-01    TO WSAA-UPPER.                      <A06259>
           MOVE T3716-CHARSET-02    TO WSAA-LOWER.                      <A06259>
           MOVE T3716-CHARSET-03    TO WSAA-SPECIAL.                    <A06259>
           MOVE T3716-CHARSET-04    TO WSAA-SPECIAL-2.                  <V4F012>
           MOVE T3716-CHARSET-05    TO WSAA-SPECIAL-3.                  <V4F012>
           MOVE T3716-CHARSET-06    TO WSAA-SPECIAL-4.                  <V4F012>
           MOVE T3716-CHARSET-07    TO WSAA-SPECIAL-5.                  <V4F012>
      *                                                                 <GAPPH2>
       2127-VALIDATE-STAFF-PRINT.                                       <GAPPH2>
      *                                                                 <GAPPH2>
           IF S2465-CANFLAG         NOT = 'Y' AND 'N' AND SPACES        <GAPPH2>
              MOVE F975             TO S2465-CANFLAG-ERR                <GAPPH2>
           END-IF.                                                      <GAPPH2>
      *                                                                 <A06259>
       2128-VALIDATE-SURNAME.                                           <A06259>
      *                                                                 <A06259>
      **** IF S2465-SURNAME            = SPACES                         <PSE30>
           IF S2465-LSURNAME           = SPACES                         <PSE30>
      ****    MOVE E186                TO S2465-SURNAME-ERR             <PSE30>
              MOVE E186                TO S2465-LSURNAME-ERR            <PSE30>
           ELSE
      ****    MOVE S2465-SURNAME       TO WSAA-CHARACTERS               <PSE30>
              MOVE S2465-LSURNAME      TO WSAA-CHARACTERS               <PSE30>
              PERFORM A3000-VALNAME
              IF VALN-STATUZ           NOT = O-K
      ****       MOVE VALN-STATUZ      TO S2465-SURNAME-ERR.            <PSE30>
                 MOVE VALN-STATUZ      TO S2465-LSURNAME-ERR.           <PSE30>
      *
      *    Given name is validated by A3000-VALNAME
      *
      **** IF CLTS-CLTIND = 'L'                                         <028>
      ****    IF S2465-GIVNAME            = SPACES                      <028>
      **** IF S2465-GIVNAME            = SPACES                 <PSE30> <028>
      ***  IF S2465-LGIVNAME            = SPACES                <GAPPH1> <PSE30>
      ****    MOVE E186               TO S2465-GIVNAME-ERR.             <PSE30>
      ***     MOVE E186               TO S2465-LGIVNAME-ERR.    <GAPPH1> <PSE30>
      *
      **** IF S2465-GIVNAME NOT = SPACES                                <PSE30>
           IF S2465-LGIVNAME NOT = SPACES                               <PSE30>
      ****    MOVE S2465-GIVNAME       TO WSAA-CHARACTERS               <PSE30>
              MOVE S2465-LGIVNAME      TO WSAA-CHARACTERS               <PSE30>
              PERFORM A3000-VALNAME
              IF VALN-STATUZ           NOT = O-K
      ****       MOVE VALN-STATUZ      TO S2465-GIVNAME-ERR.            <PSE30>
                 MOVE VALN-STATUZ      TO S2465-LGIVNAME-ERR.           <PSE30>
      *
      *    Check middle names if entered by A3000-VALNAME
      *
      **** IF S2465-MIDDL  (1)  NOT = SPACES                            <PSE30>
      ****    MOVE S2465-MIDDL  (1)    TO WSAA-CHARACTERS               <PSE30>
      ****    PERFORM A3000-VALNAME                                     <PSE30>
      ****    IF VALN-STATUZ           NOT = O-K                        <PSE30>
      ****       MOVE VALN-STATUZ      TO S2465-MIDDL-ERR (1).          <PSE30>
      *
      **** IF S2465-MIDDL  (2)  NOT = SPACES                            <PSE30>
      ****    MOVE S2465-MIDDL  (2)    TO WSAA-CHARACTERS               <PSE30>
      ****    PERFORM A3000-VALNAME                                     <PSE30>
      ****    IF VALN-STATUZ           NOT = O-K                        <PSE30>
      ****       MOVE VALN-STATUZ      TO S2465-MIDDL-ERR (2).          <PSE30>
      *                                                                 <028>
      *    Salutation Must be mandatory                                 <028>
      *                                                                 <028>
      ****    IF S2465-SALUTL             = SPACES              <WLI.P2><028>
      ****    IF S2465-SALUT              = SPACES              <WLI.P2><CAS2.0>
      ****    IF S2465-SALUTL             = SPACES              <FA2737><CAS2.0>
      ****       MOVE E186               TO S2465-SALUTL-ERR.   <WLI.P2><028>
      ****       MOVE E186               TO S2465-SALUT-ERR.    <WLI.P2><CAS2.0>
      ****       MOVE E186               TO S2465-SALUTL-ERR.   <FA2737><CAS2.0>
      *                                                                 <GAPPH1>
      *    SoE must be mandatory                                        <GAPPH1>
      *                                                                 <GAPPH1>
      **** IF S2465-SOE                = SPACES                 <GAPPH2><GAPPH1>
      ****     MOVE E186               TO S2465-SOE-ERR         <GAPPH2><GAPPH1>
      **** END-IF.                                              <GAPPH2><GAPPH1>
      *                                                                 <GAPPH2>
      * ID Issued Date & Place must be mandatory:                       <GAPPH2>
      *                                                                 <GAPPH2>
           IF S2465-IDDATE             = ZEROES OR VRCM-MAX-DATE        <GAPPH2>
               MOVE E186               TO S2465-IDDATE-ERR              <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
      * ID Date must be >= 01/01/1990:                                  <PHE025>
           IF S2465-IDDATE             > ZEROES                         <PHE025>
           AND S2465-IDDATE            < WSAA-VAL-IDDATE                <PHE025>
               MOVE D020               TO S2465-IDDATE-ERR              <PHE025>
           END-IF.                                                      <PHE025>
                                                                        <PHE025>
           IF S2465-IDPLACE            = SPACES                         <GAPPH2>
           AND S2465-IDPLACEXT         = SPACES                         <CLM14>
               MOVE E186               TO S2465-IDPLACE-ERR             <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH1>
PHI   **** IF S2465-ZPTCITY            = SPACES                 <NB020> <GAPPH1>
      ****     MOVE H366               TO S2465-ZPTCITY-ERR     <NB020> <GAPPH1>
      **** END-IF.                                              <NB020> <GAPPH1>
                                                                        <NB020>
      **** IF S2465-ZPTDIST            = SPACES                 <NB020> <GAPPH1>
      ****     MOVE H366               TO S2465-ZPTDIST-ERR     <NB020> <GAPPH1>
      **** END-IF.                                              <NB020> <GAPPH1>
                                                                        <GAPPH1>
      **** IF S2465-ZPTWARD            = SPACES                 <NB020> <GAPPH1>
      ****     MOVE H366               TO S2465-ZPTWARD-ERR     <NB020> <GAPPH1>
      **** END-IF.                                              <NB020> <GAPPH1>
                                                                        <NB020>
           IF  S2465-ZPTCITY           = SPACES                         <NB020>
           AND S2465-CLTADDR-05    NOT = SPACES                         <NB020>
               MOVE G987               TO S2465-CLTADDR05-ERR           <NB020>
           END-IF.                                                      <NB020>
                                                                        <NB020>
           IF  S2465-ZPTDIST           = SPACES                         <NB020>
           AND S2465-CLTADDR-04    NOT = SPACES                         <NB020>
               MOVE G987               TO S2465-CLTADDR04-ERR           <NB020>
           END-IF.                                                      <NB020>
                                                                        <NB020>
           IF  S2465-ZPTWARD           = SPACES                         <NB020>
           AND S2465-CLTADDR-03    NOT = SPACES                         <NB020>
               MOVE G987               TO S2465-CLTADDR03-ERR           <NB020>
           END-IF.                                                      <NB020>
      *                                                                 <NB020>
           IF  S2465-CLTADDR-01        = SPACES                         <NB020>
           AND S2465-CLTADDR-02    NOT = SPACES                         <NB020>
               MOVE G987               TO S2465-CLTADDR01-ERR           <NB020>
           END-IF.                                                      <NB020>
      *                                                                 <NB020>
           IF   S2465-ZPTCITY      NOT = SPACES                         <NB020>
           AND (S2465-ZPTDIST          = SPACES                         <NB020>
           OR   S2465-ZPTWARD          = SPACES)                        <NB020>
                MOVE G987              TO S2465-ZPTDIST-ERR             <NB020>
                                          S2465-ZPTCITY-ERR             <NB020>
                                          S2465-ZPTWARD-ERR             <NB020>
           END-IF.                                                      <NB020>
      *                                                                 <NB020>
           IF (S2465-ZPTCITY           = SPACES                         <NB020>
           OR  S2465-ZPTDIST           = SPACES                         <NB020>
           OR  S2465-ZPTWARD           = SPACES)                        <NB020>
               IF S2465-CLTADDR-01 NOT = SPACES                         <NB020>
               MOVE G987               TO S2465-CLTADDR01-ERR           <NB020>
                                          S2465-CLTADDR02-ERR           <NB020>
                                          S2465-CLTADDR03-ERR           <NB020>
                                          S2465-CLTADDR04-ERR           <NB020>
                                          S2465-CLTADDR05-ERR           <NB020>
               END-IF                                                   <NB020>
           ELSE                                                         <NB020>
               IF S2465-CLTADDR-01     = SPACES                         <NB020>
                  MOVE G987               TO S2465-CLTADDR01-ERR        <NB020>
               END-IF                                                   <NB020>
PHI        END-IF.                                                      <NB020>
      *                                                         <CL002> <GVL909>
      * Check valid District & City; Ward & District:           <CL002> <GVL909>
      *                                                                 <GAPPH1>
           IF S2465-ZPTDIST            NOT = SPACES                     <GAPPH1>
               MOVE SPACES             TO LOC-LOCATION-REC              <GAPPH1>
               MOVE S2465-ZPTCITY      TO LOC-CITY-CODE                 <GAPPH1>
               MOVE S2465-ZPTDIST      TO LOC-DIST-CODE                 <GAPPH1>
               MOVE 'DIST'             TO LOC-FUNCTION                  <GAPPH1>
               CALL 'ZLOCCHK'          USING LOC-LOCATION-REC           <GAPPH1>
               IF LOC-VALID-CODE       NOT = 'Y'                        <GAPPH1>
                   MOVE LOC-ERROR-CODE TO S2465-ZPTDIST-ERR             <GAPPH1>
               ELSE                                                     <GAPPH1>
                   MOVE SPACES         TO S2465-ZPTDIST-ERR             <GAPPH1>
               END-IF                                                   <GAPPH1>
           END-IF.                                                      <GAPPH1>
                                                                        <GAPPH1>
           IF S2465-ZPTWARD            NOT = SPACES                     <GAPPH1>
               MOVE SPACES             TO LOC-LOCATION-REC              <GAPPH1>
               MOVE S2465-ZPTDIST      TO LOC-DIST-CODE                 <GAPPH1>
               MOVE S2465-ZPTWARD      TO LOC-WARD-CODE                 <GAPPH1>
               MOVE 'WARD'             TO LOC-FUNCTION                  <GAPPH1>
               CALL 'ZLOCCHK'          USING LOC-LOCATION-REC           <GAPPH1>
               IF LOC-VALID-CODE       NOT = 'Y'                        <GAPPH1>
                   MOVE LOC-ERROR-CODE TO S2465-ZPTWARD-ERR             <GAPPH1>
               ELSE                                                     <GAPPH1>
                   MOVE SPACES         TO S2465-ZPTWARD-ERR             <GAPPH1>
               END-IF                                                   <GAPPH1>
           END-IF.                                                      <GAPPH1>
                                                                        <GAPPH1>
           IF S2465-ZPTCITY        NOT = SPACES                         <GAPPH1>
           AND S2465-ZPTDIST       NOT = SPACES                         <GAPPH1>
           AND S2465-ZPTWARD       NOT = SPACES                         <GAPPH1>
           AND S2465-CLTADDR-05        = SPACES                         <GAPPH1>
               PERFORM 2300-SHOW-ADDRESS-LINES                          <GAPPH1>
               MOVE 'Y'                TO WSSP-EDTERROR                 <GAPPH1>
           END-IF.                                                      <GAPPH1>
      *
      *    Address is checked by call to VALADDR
      *
           MOVE VALN-ADDRESS           TO VALN-FUNCTION.
      **** IF S2465-CLTADDR (1)       = SPACES                          <030>
      ****    MOVE E110                TO S2465-CLTADDR01-ERR           <030>
      **** ELSE                                                         <030>
      ****    MOVE S2465-CLTADDR (1)   TO WSAA-CHARACTERS               <030>
      ****    PERFORM A3000-VALNAME                                     <030>
      ****    IF VALN-STATUZ           NOT = O-K                        <030>
      ****       MOVE VALN-STATUZ      TO S2465-CLTADDR-ERR (1).        <030>

       2129-ADDRESS.                                                    <GAPPH1>
                                                                        <GAPPH1>
           IF S2465-CLTADDR (1)  NOT  = SPACES                          <033>
              MOVE S2465-CLTADDR (1)   TO WSAA-CHARACTERS               <033>
              PERFORM A3000-VALNAME                                     <033>
              IF VALN-STATUZ            = G986                          <FA3226>
                 MOVE G987             TO VALN-STATUZ                   <FA3226>
              END-IF                                                    <FA3226>
              IF VALN-STATUZ           NOT = O-K                        <033>
                 MOVE VALN-STATUZ      TO S2465-CLTADDR-ERR (1).        <033>
      *
           IF S2465-CLTADDR (2)  NOT  = SPACES
              MOVE S2465-CLTADDR (2)   TO WSAA-CHARACTERS
              PERFORM A3000-VALNAME
              IF VALN-STATUZ            = G986                          <FA3226>
                 MOVE G987             TO VALN-STATUZ                   <FA3226>
              END-IF                                                    <FA3226>
              IF VALN-STATUZ           NOT = O-K
                 MOVE VALN-STATUZ      TO S2465-CLTADDR-ERR (2).
      *
           IF S2465-CLTADDR  (3)  NOT  = SPACES
              MOVE S2465-CLTADDR (3)   TO WSAA-CHARACTERS
              PERFORM A3000-VALNAME
              IF VALN-STATUZ            = G986                          <FA3226>
                 MOVE G987             TO VALN-STATUZ                   <FA3226>
              END-IF                                                    <FA3226>
              IF VALN-STATUZ           NOT = O-K
                 MOVE VALN-STATUZ      TO S2465-CLTADDR-ERR (3).
      *
           IF S2465-CLTADDR (4)  NOT  = SPACES
              MOVE S2465-CLTADDR (4)   TO WSAA-CHARACTERS
              PERFORM A3000-VALNAME
              IF VALN-STATUZ            = G986                          <FA3226>
                 MOVE G987             TO VALN-STATUZ                   <FA3226>
              END-IF                                                    <FA3226>
              IF VALN-STATUZ           NOT = O-K
                 MOVE VALN-STATUZ      TO S2465-CLTADDR-ERR (4).
      *
           IF S2465-CLTADDR (5)  NOT  = SPACES
              MOVE S2465-CLTADDR (5)   TO WSAA-CHARACTERS
              PERFORM A3000-VALNAME
              IF VALN-STATUZ            = G986                          <FA3226>
                 MOVE G987             TO VALN-STATUZ                   <FA3226>
              END-IF                                                    <FA3226>
              IF VALN-STATUZ           NOT = O-K
                 MOVE VALN-STATUZ      TO S2465-CLTADDR-ERR (5).
      *
           IF  VALN-STATUZ NOT = O-K
               GO TO 2130-NEXT.
      *
      **** IF  S2465-CLTADDR (2)  = SPACES                              <030>
      ****     IF  S2465-CLTADDR (3)  NOT  = SPACES                     <030>
      ****     OR  S2465-CLTADDR (4)  NOT  = SPACES                     <030>
      ****     OR  S2465-CLTADDR (5)  NOT  = SPACES                     <030>
      ****         MOVE H366             TO S2465-CLTADDR-ERR (2).      <030>
      *
      **** IF  S2465-CLTADDR (3)  = SPACES                              <030>
      ****     IF  S2465-CLTADDR (4)  NOT  = SPACES                     <030>
      ****     OR  S2465-CLTADDR (5)  NOT  = SPACES                     <030>
      ****         MOVE H366             TO S2465-CLTADDR-ERR (3).      <030>
      *
      **** IF  S2465-CLTADDR (4)  = SPACES                              <030>
      ****     IF  S2465-CLTADDR (5)  NOT  = SPACES                     <030>
      ****         MOVE H366             TO S2465-CLTADDR-ERR (4).      <030>
      *
           IF S2465-CTRYCODE           NOT = WSAA-CLTS-CTRYCODE         <A05912>
           MOVE S2465-CTRYCODE         TO WSAA-CLTS-CTRYCODE            <A05912>
               MOVE 'Y'                TO WSAA-COUNTRY                  <A05912>
           END-IF.                                                      <A05912>
                                                                        <A05912>
           IF WSSP-LANGUAGE         NOT = WSAA-SIGNON-LANG              <A05912>
               MOVE WSSP-LANGUAGE      TO WSAA-SIGNON-LANG              <A05912>
               MOVE 'Y'                TO WSAA-LANG-CHANGED             <A05912>
           END-IF.                                                      <A05912>
      *                                                                 <A05912>
           PERFORM 6100-VALIDATE-ADDRESS.                               <030>
      *                                                                 <030>
TDO   * Check Duplicate Address for Action A and C:                     <NB019>
                                                                        <NB019>
           IF (WSSP-SBMACTION          = 'A' OR 'C')                    <NB019>
           AND S2465-ZPTCITY           NOT = SPACES                     <NB019>
           AND S2465-ZPTDIST           NOT = SPACES                     <NB019>
           AND S2465-ZPTWARD           NOT = SPACES                     <NB019>
           AND S2465-CLTADDR (1)       NOT = SPACES                     <NB019>
               PERFORM X1000-CHECK-ADDRESS-DUP                          <NB019>
               IF (WSAA-ADDRESS-DUP     = 'Y'                           <NB019>
               AND WSAA-ADDR-WARN      NOT = 'Y')                       <NB019>
               OR (WSAA-ADDRESS-DUP     = 'Y'                           <NB019>
               AND WSAA-ADDRLINE12     NOT = WSAA-ADDRLINE12-BK)        <NB019>
                  MOVE EV98            TO S2465-CLTADDR01-ERR           <NB019>
                                          S2465-CLTADDR02-ERR           <NB019>
                                          S2465-CLTADDR03-ERR           <NB019>
                                          S2465-CLTADDR04-ERR           <NB019>
                                          S2465-CLTADDR05-ERR           <NB019>
                  MOVE 'Y'             TO WSAA-ADDR-WARN                <NB019>
                  MOVE WSAA-ADDRLINE12 TO WSAA-ADDRLINE12-BK            <NB019>
                  MOVE WSAA-CLNTLINE12 TO WSAA-CLNTLINE12-BK            <NB019>
               END-IF                                                   <NB019>
           END-IF.                                                      <NB019>
                                                                        <NB008>
           IF WSSP-SBMACTION           = 'A'                            <NB008>
           OR WSSP-SBMACTION           = 'C'                            <NB008>
               IF S2465-RMBLPHONE      NOT = SPACES                     <NB008>
                  PERFORM 2600-READ-CLEXMBP                             <NB008>
               END-IF                                                   <NB008>
                                                                        <NB008>
               IF S2465-CLTPHONE-02    NOT = SPACES                     <NB008>
                  PERFORM C2600-READ-CLNTOFP                            <NB008>
               END-IF                                                   <NB008>
                                                                        <NB008>
               IF S2465-CLTPHONE-01    NOT = SPACES                     <NB008>
                  PERFORM D2600-READ-CLNTHSP                            <NB008>
               END-IF                                                   <NB008>
           END-IF.                                                      <NB008>
                                                                        <NB008>
       2130-NEXT.
      *
      *    Default address type to R (must be an entry on T3690!)
      *    Hardcoding is inappropriate to non-English clients!!         (031>
      *
           IF S2465-ADDRTYPE = SPACE
      ****    MOVE 'R'                 TO S2465-ADDRTYPE.               <031>
              MOVE E186                TO S2465-ADDRTYPE-ERR.           <031>
      *
      *    Servicing branch checked on table by IO module
      *    only if entered but spaces are not allowed
      *
           IF S2465-SERVBRH            = SPACES
              MOVE E186                TO S2465-SERVBRH-ERR.

      *
      *    S2465-CLTSEX is a mandatory field and must be validated
      *     against T3582
      *
           IF S2465-CLTSEX             = SPACES
              MOVE H366                TO S2465-CLTSEX-ERR.

       2135-MARRYD-STAT.
      *
      *    S2465-MARRYD is a mandatory field and must be validated so
      *    The validation against T3571 is done automatically thru the
      *    field dictionary definition.
      *
           IF S2465-MARRYD             = SPACES
              MOVE H366                TO S2465-MARRYD-ERR.

       2145-COUNTRY.
      *
      *
      *    Country code checked on table by IO module only if
      *    entered but spaces are not allowed
      *
           IF S2465-CTRYCODE           = SPACES
              MOVE E186                TO S2465-CTRYCODE-ERR.
      *
           IF S2465-CTRYCODE-ERR       NOT = SPACES
              GO TO 2100-CONT.
      *                                                                 <A06259>
       2155-LANGUAGE.                                                   <A06259>
      *                                                                 <A06259>
      *                                                                 <A06259>
      *    Language code of spaces no longer allowed.                   <A06259>
      *                                                                 <A06259>
           IF S2465-LANGUAGE           = SPACES                         <A06259>
           OR                          NOT = 'E'                        <NB006>
              MOVE E186                TO S2465-LANGUAGE-ERR.           <A06259>
      *                                                                 <A06259>
           IF S2465-LANGUAGE-ERR       NOT = SPACES                     <A06259>
              GO TO 2100-CONT.                                          <A06259>
      *
       2200-NEXT.
      *
      * Let the postcode validation specified on the country code
      * table decide whether a postcode of spaces is allowed.
      *
      **** IF S2465-CLTPCODE = SPACES
      ****     GO TO 2100-CONT.

           MOVE SPACES                 TO ITEM-PARAMS.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.
           MOVE T3645                  TO ITEM-ITEMTABL.
           MOVE S2465-CTRYCODE         TO ITEM-ITEMITEM.
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO'               USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR.

           MOVE ITEM-GENAREA           TO T3645-T3645-REC.
      *
      * If the postcode isn't entered and the country code table defines
      * it mandatory, 'M', error,highlighting the field must be entered.
      * However, if the table defines the postcode as optional,'O', skip
      * the validation modules if the postcode is not entered.
      * On the other hand if the postcode is entered and the table says
      * it shouldn't be, 'N', this too is an error.
      *
           IF S2465-CLTPCODE           = SPACES
               IF T3645-INDIC          = 'M'
                   MOVE E186           TO S2465-CLTPCODE-ERR
                   GO 2100-CONT
               END-IF
               IF T3645-INDIC          = 'O'
                   GO 2100-CONT
               END-IF
               IF T3645-INDIC          = 'N'
                   GO 2100-CONT
               END-IF
           ELSE
                IF T3645-INDIC         = 'N'
                    MOVE E374          TO S2465-CLTPCODE-ERR
                    GO 2100-CONT
                END-IF
            END-IF.
      *
      * If there is a module entered on the postcode table
      * call it to validate.
      *
           IF T3645-PCODE-VAL-MOD      = SPACES
              GO TO 2100-CONT.

           MOVE S2465-CLTPCODE         TO PCHK-POSTCODE.
           MOVE S2465-CTRYCODE         TO PCHK-CTRYCODE.
           MOVE S2465-CLTADDR (1)      TO PCHK-ADDR1.
           MOVE S2465-CLTADDR (2)      TO PCHK-ADDR2.
           MOVE S2465-CLTADDR (3)      TO PCHK-ADDR3.
           MOVE S2465-CLTADDR (4)      TO PCHK-ADDR4.
           MOVE S2465-CLTADDR (5)      TO PCHK-ADDR5.
           MOVE T3645-DISTSIZE         TO PCHK-DISTSIZE.
           MOVE WSSP-FSUCO             TO PCHK-COMPANY.
      *
           CALL T3645-PCODE-VAL-MOD    USING PCHK-PCODEREC-REC.
           IF PCHK-STATUZ              NOT = O-K
              MOVE PCHK-STATUZ         TO S2465-CLTPCODE-ERR
              MOVE 'Y'                 TO WSSP-EDTERROR.

      *    MOVE SPACES                 TO CKPC-CHKPOST-REC.
      *    MOVE WSSP-FSUCO             TO CKPC-COMPNY.
      *    MOVE S2465-CLTPCODE         TO CKPC-PCODE.
      *    MOVE S2465-CTRYCODE         TO CKPC-COUNTRY.
      *    MOVE 'CHKSZE'               TO CKPC-FUNCTION.
      *    CALL 'CHKPOST'              USING CKPC-CHKPOST-REC.
      *
      *    IF CKPC-STATUZ              NOT = O-K
      *       MOVE CKPC-STATUZ         TO S2465-CLTPCODE-ERR.
      *
       2100-CONT.
      *
      *    Client title MAY BE BLANKS
      *
      **** IF S2465-SALUT            = SPACES                   <WLI.P2><CAS2.0>
           IF S2465-SALUTL           = SPACES                           <CAS2.0>
              GO TO 2150-COMP.
      *
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.
           MOVE T3583                  TO ITEM-ITEMTABL.
      **** MOVE S2465-SALUT            TO ITEM-ITEMITEM.        <WLI.P2><CAS2.0>
           MOVE S2465-SALUTL           TO ITEM-ITEMITEM.                <CAS2.0>
      **** IF S2465-LANGUAGE            = 'T'                           <FA2751>
      ****    GO TO 2145-CALL-T3583                                     <FA2751>
      **** END-IF.                                                      <FA2751>
      *
      *  Salutation table entry will be in upper case.
      *
      **** CALL 'QDCXLATE'             USING WSAA-ITEM-LENGTH           <FA2751>
      ****                                   ITEM-ITEMITEM              <FA2751>
      ****                                   WSAA-QCASE256              <FA2751>
      ****                                   WSAA-QUSRSYS.              <FA2751>
      *
       2145-CALL-T3583.                                                 <FA2751>
                                                                        <FA2751>
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO'               USING ITEM-PARAMS
           IF ITEM-STATUZ              NOT = O-K
                                  AND  NOT = MRNF
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF ITEM-STATUZ = MRNF
      ****    MOVE G984               TO S2465-SALUTL-ERR               <WLI.P2>
      ****    MOVE G984               TO S2465-SALUT-ERR        <WLI.P2><CAS2.0>
              MOVE G984               TO S2465-SALUTL-ERR               <CAS2.0>
              GO TO 2150-COMP.
      *
           MOVE ITEM-GENAREA          TO T3583-T3583-REC.
      *
           IF  T3583-CLTSEX NOT = SPACES
           AND S2465-CLTSEX NOT = SPACES
               IF  T3583-CLTSEX  NOT = S2465-CLTSEX
      ****         MOVE G983     TO    S2465-SALUTL-ERR                 <WLI.P2>
      ****         MOVE G983     TO    S2465-SALUT-ERR          <WLI.P2><CAS2.0>
                   MOVE G983     TO    S2465-SALUTL-ERR                 <CAS2.0>
                                       S2465-CLTSEX-ERR.
      *
       2150-COMP.
      * Nationality is Mandatory.                                       <V4F001>
           IF S2465-NATLTY             = SPACES                         <V4F001>
              MOVE E186                TO S2465-NATLTY-ERR              <V4F001>
              GO TO 2000-CONT                                           <V4F001>
           END-IF.                                                      <V4F001>
                                                                        <V4F001>

           MOVE SPACES                 TO ITEM-PARAMS.                  <GRP2.0>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <GRP2.0>
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.                 <GRP2.0>
           MOVE T3645                  TO ITEM-ITEMTABL.                <GRP2.0>
           MOVE S2465-NATLTY           TO ITEM-ITEMITEM.                <GRP2.0>
           MOVE READR                  TO ITEM-FUNCTION.                <GRP2.0>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <GRP2.0>
           IF ITEM-STATUZ              NOT = O-K AND                    <GRP2.0>
              ITEM-STATUZ              NOT = MRNF                       <GRP2.0>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <GRP2.0>
              PERFORM 600-FATAL-ERROR.                                  <GRP2.0>
                                                                        <GRP2.0>
           IF ITEM-STATUZ                  = MRNF                       <GRP2.0>
              INITIALIZE  T3645-T3645-REC                               <GRP2.0>
           ELSE                                                         <GRP2.0>
              MOVE ITEM-GENAREA        TO T3645-T3645-REC               <GRP2.0>
           END-IF.                                                      <GRP2.0>
                                                                        <GRP2.0>
           IF S2465-SECUITYNO           = SPACES                        <CAS1.0>
           AND T3645-ZRMANDIND          = 'Y'                           <CAS1.0>
              MOVE RF02                TO S2465-SECUITYNO-ERR           <CAS1.0>
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *       GO TO 2900-EXIT                                           <S9503>>
      ****    GO TO 2090-EXIT                                   <PSE30> <S9503>>
              GO TO 2000-CONT                                           <PSE30>
           END-IF.                                                      <CAS1.0>
                                                                        <V62P07>
           MOVE S2465-SECUITYNO        TO WSAA-OLD-SECUITYNO.           <V62P07>
           MOVE SPACES                 TO ZRIDCHKREC-REC.               <V62P07>
           IF S2465-SECUITYNO          NOT = WSAA-SECUITYNO             <V62P07>
              MOVE SPACES              TO MBOX-REPLY                    <V62P07>
              MOVE S2465-SECUITYNO     TO WSAA-SECUITYNO                <V62P07>
              MOVE 'Y'                 TO WSAA-ID-CHANGED               <V65F10>
           END-IF.                                                      <V62P07>

      *--Validate ID number if T3645-ZRMANDIND = 'Y' And                <V6F104>
      *  T3645-ZRNISUBR = Spaces                                        <V6F104>
           IF T3645-ZRMANDIND           = 'Y' AND                       <V6F104>
              T3645-ZRNISUBR            = SPACES                        <V6F104>
              GO TO 2165-COMP                                           <V6F104>
           END-IF.                                                      <V6F104>
                                                                        <CAS1.0>
           IF S2465-SECUITYNO           = SPACES                        <CAS1.0>
           OR T3645-ZRNISUBR            = SPACES                        <CAS1.0>
           OR (T3645-ZRMANDIND      NOT = 'Y' AND 'O')                  <CAS1.0>
               GO TO 2160-COMP                                          <CAS1.0>
           END-IF.                                                      <CAS1.0>
                                                                        <CAS1.0>
      * Check for Dummy ID                                              <V4F001>
           MOVE S2465-SECUITYNO         TO WSAA-DUMMY-ID.               <V4F001>
           IF  DUMMY-1                   = DUMMY-2                      <V4F001>
           AND DUMMY-1                   = DUMMY-3                      <V4F001>
           AND DUMMY-1                   = DUMMY-4                      <V4F001>
           AND DUMMY-1                   = DUMMY-5                      <V4F001>
           AND WSAA-DUMMY-ID ALPHABETIC                                 <V4F001>
               GO TO 2160-COMP                                          <V4F001>
           END-IF.                                                      <V4F001>
                                                                        <V4F001>
      **** IF  S2465-SECUITYNO  = SPACES                                <CAS1.0>
      ****     GO TO 2160-COMP.                                         <CAS1.0>
                                                                        <025>
           IF  CLTS-CLTIND             = 'D' OR 'L'                     <025>
               GO TO 2160-COMP.                                         <025>
      *                                                                 <CAS1.0>
      ***  MOVE SPACES                 TO ZRIDCHKREC-REC.       <V62P07><CAS1.0>
           MOVE S2465-SECUITYNO        TO ZRIDCHK-SECUITYNO.            <CAS1.0>
           MOVE S2465-NATLTY           TO ZRIDCHK-CTRYCODE.             <V74F03>
           MOVE S2465-CLTDOBX          TO ZRIDCHK-DOB.                  <V74F03>
           MOVE S2465-CLTSEX           TO ZRIDCHK-SEX.                  <V74F03>
           MOVE WSSP-FSUCO             TO ZRIDCHK-FSUCO.                <V74F03>
           CALL T3645-ZRNISUBR         USING ZRIDCHKREC-REC.            <CAS1.0>
                                                                        <CAS1.0>
      **** IF ZRIDCHK-STATUZ       NOT = O-K                    <V6F104><CAS1.0>
      ****    MOVE RF01               TO S2465-SECUITYNO-ERR    <V6F104><CAS1.0>
      **** ELSE                                                 <V6F104><CAS1.0>
      ****    MOVE ZRIDCHK-SECUITYNO-OUT TO S2465-SECUITYNO     <V6F104><CAS1.0>
      **** END-IF.                                              <V6F104>,CAS1.0.
                                                                        <CAS1.0>
      **** IF ZRIDCHK-STATUZ           NOT = O-K AND 'SKIP'     <FA2773><V6F104>
           IF SCRN-DEVICE-IND          NOT = '*RMT'                     <FA2773>
           AND (ZRIDCHK-STATUZ         NOT = O-K AND 'SKIP')            <FA2773>
      **** Add error message if sex on ID is not match with Person sex  <V74F03>
              IF                                                        <V74F03>
                ZRIDCHK-STATUZ          = RF01 OR                       <V74F03>
                ZRIDCHK-STATUZ          = RGD6                          <V74F03>
              THEN                                                      <V74F03>
                 MOVE ZRIDCHK-STATUZ  TO S2465-SECUITYNO-ERR            <V74F03>
              ELSE                                                      <V74F03>
                 MOVE RF01            TO S2465-SECUITYNO-ERR            <V74F03>
              END-IF                                                    <V74F03>
      *****   MOVE RF01                TO S2465-SECUITYNO-ERR   <V74F03><V6F104>
      ***** Always Display error when IC validation fail                <V74F03>
      *****   IF WSAA-WARNING-COUNT = 1                         <V74F03><V62P07>
      *****      MOVE 'Y'              TO WSSP-EDTERROR         <V74F03><V62P07>
      *****      ADD 1                 TO WSAA-WARNING-COUNT    <V74F03><V62P07>
      *****      MOVE S2465-SECUITYNO  TO WSAA-IVID-SECUITYNO   <V74F03><V62P07>
      ****       GO TO 2090-EXIT                                <FA2737><V62P07>
      *****      GO TO 2000-CONT                                <V74F03><FA2737>
      *****   ELSE                                              <V74F03><V62P07>
      *****      IF S2465-SECUITYNO    NOT = WSAA-IVID-SECUITYNO<V74F03><V62P07>
      *****         MOVE 'Y'           TO WSSP-EDTERROR         <V74F03><V62P07>
      *****         MOVE S2465-SECUITYNO TO WSAA-IVID-SECUITYNO <V74F03><V62P07>
      ****          GO TO 2090-EXIT                             <FA2737><V62P07>
      *****         GO TO 2000-CONT                             <V74F03><FA2737>
      *****      ELSE                                           <V74F03><V62P07>
      *****         MOVE SPACES        TO S2465-SECUITYNO-ERR   <V74F03><V62P07>
      *****      END-IF                                         <V74F03><V62P07>
      *****   END-IF                                            <V74F03><V62P07>
           ELSE                                                         <V6F104>
              IF ZRIDCHK-STATUZ        = 'SKIP'                         <V6F104>
                 GO TO 2160-COMP                                        <V6F104>
              ELSE                                                      <V6F104>
                 MOVE ZRIDCHK-SECUITYNO-OUT                             <V6F104>
                                      TO S2465-SECUITYNO                <V6F104>
              END-IF                                                    <V6F104>
           END-IF.                                                      <V6F104>

       2165-COMP.                                                       <CAS1.0>
      *                                                                 <V62P07>
      *  After return back from calling subroutine and ID is invalid    <V62P07>
      *  the value becomes spaces so moving the old value back          <V62P07>
      *                                                                 <V62P07>
           IF ZRIDCHK-STATUZ       NOT = O-K AND                        <V62P07>
              S2465-SECUITYNO          = SPACES                         <V62P07>
              MOVE WSAA-OLD-SECUITYNO TO S2465-SECUITYNO                <V62P07>
              MOVE RF01               TO S2465-SECUITYNO-ERR            <V74F03>
              MOVE 'Y'                TO WSSP-EDTERROR                  <V62P07>
      ****    GO TO 2090-EXIT                                   <FA2737><V62P07>
              GO TO 2000-CONT                                           <FA2737>
           END-IF.                                                      <V62P07>
                                                                        <V62P07>
           INITIALIZE                  WSKY-CLNTSSN-KEY.                <V62P07>
      ***  MOVE S2465-SECUITYNO           TO CLNTSSN-SECUITYNO.         <V62P07>
           MOVE S2465-SECUITYNO        TO WSKY-CLNTSSN-SECUITYNO.       <V62P07>
           MOVE WSKY-CLNTSSN-FILE-KEY  TO CLNTSSN-DATA-KEY.             <V62P07>
           MOVE CLNTSSNREC                TO CLNTSSN-FORMAT.
      ***  MOVE READR                     TO CLNTSSN-FUNCTION.          <V62P07>
           MOVE BEGN                   TO CLNTSSN-FUNCTION.             <V62P07>
           MOVE O-K                    TO CLNTSSN-STATUZ.               <V62P07>
           MOVE 'N'                    TO WSAA-CLNTSSN-FND.             <V62P07>
      ***  CALL 'CLNTSSNIO'            USING CLNTSSN-PARAMS.            <V62P07>
      ***  IF  CLNTSSN-STATUZ          NOT = O-K                        <V62P07>
      ***  AND CLNTSSN-STATUZ          NOT = MRNF                       <V62P07>
      ***      MOVE WSKY-CLNTSSN-FILE-KEY TO CLNTSSN-DATA-KEY           <V62P07>
      ***      MOVE CLNTSSN-PARAMS      TO SYSR-PARAMS                  <V62P07>
      ***      PERFORM 600-FATAL-ERROR.                                 <V62P07>

           PERFORM UNTIL CLNTSSN-STATUZ NOT = O-K                       <V62P07>
             OR          WSAA-CLNTSSN-FND = 'Y'                         <V62P07>
             CALL 'CLNTSSNIO'          USING CLNTSSN-PARAMS             <V62P07>
             IF CLNTSSN-STATUZ         NOT = O-K AND ENDP               <V62P07>
                MOVE WSKY-CLNTSSN-FILE-KEY TO CLNTSSN-DATA-KEY          <V62P07>
                MOVE CLNTSSN-PARAMS    TO SYSR-PARAMS                   <V62P07>
                PERFORM 600-FATAL-ERROR                                 <V62P07>
             END-IF                                                     <V62P07>
             IF CLNTSSN-STATUZ         NOT = O-K                        <V62P07>
             OR CLNTSSN-SECUITYNO      NOT = WSKY-CLNTSSN-SECUITYNO     <V62P07>
                MOVE MRNF              TO CLNTSSN-STATUZ                <V62P07>
             END-IF                                                     <V62P07>
             IF  CLNTSSN-STATUZ        = O-K                            <V62P07>
             AND CLNTSSN-SECUITYNO     = WSKY-CLNTSSN-SECUITYNO         <V62P07>
             AND CLNTSSN-CLNTNUM       NOT = S2465-CLNTNUM              <V62P07>
             AND CLNTSSN-CLTIND        = 'C'                            <FA3292>
                MOVE 'Y'               TO WSAA-CLNTSSN-FND              <V62P07>
             END-IF                                                     <V62P07>
             MOVE NEXTR                TO CLNTSSN-FUNCTION              <V62P07>
           END-PERFORM.                                                 <V62P07>
           IF  WSSP-FLAG               = 'M'                            <V62P07>
              GO TO 2165-MODIFY                                         <V62P07>
           END-IF.                                                      <V62P07>
                                                                        <V62P07>
           IF  CLNTSSN-STATUZ          NOT = MRNF
           AND CLNTSSN-CLNTNUM         NOT = S2465-CLNTNUM
           AND T3645-DUPNO             NOT = 'Y'                        <FA4126>
               MOVE B369               TO S2465-SECUITYNO-ERR.
                                                                        <V62P07>
           GO TO 2160-COMP.                                             <V62P07>
                                                                        <V62P07>
       2165-MODIFY.                                                     <V62P07>
                                                                        <V62P07>
           IF S2465-NATLTY             NOT = WSAA-NATLTY                <V62P07>
              MOVE SPACES              TO MBOX-REPLY                    <V62P07>
              MOVE S2465-NATLTY        TO WSAA-NATLTY                   <V62P07>
           END-IF.                                                      <V62P07>
           IF  CLNTSSN-STATUZ          = O-K                            <V62P07>
           AND CLNTSSN-CLNTNUM         NOT = S2465-CLNTNUM              <V62P07>
               IF  MBOX-REPLY          NOT = 'Y' AND 'N'                <V62P07>
               AND T3645-DUPNO         = 'Y'                            <V62P07>
                  PERFORM A6000-CALL-MSGBOX                             <V62P07>
               END-IF                                                   <V62P07>
               IF  MBOX-REPLY          NOT = 'Y'                        <V62P07>
               OR  T3645-DUPNO         NOT = 'Y'                        <V62P07>
                  MOVE B369            TO S2465-SECUITYNO-ERR           <V62P07>
               END-IF                                                   <V62P07>
           END-IF.                                                      <V62P07>
           IF WSSP-SBMACTION         = 'C'                              <DA002>
              PERFORM 2800-READ-CLRRWIN                                 <DA002>
           END-IF.                                                      <DA002>
       2160-COMP.

      *
      *    Get and display statistics code description
      *
           IF S2465-STATCODE-ERR       NOT = SPACES
              GO TO 2400-CONT.
      *
           IF S2465-STATCODE           = SPACES
      ****     MOVE SPACES             TO S2465-STATDSC                 <025>
               GO TO 2400-CONT.
      *
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE 'IT'                   TO ITEM-ITEMPFX.
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.
           MOVE T3628                  TO ITEM-ITEMTABL.
           MOVE S2465-STATCODE         TO ITEM-ITEMITEM.
           PERFORM A2000-GETDESC.
           MOVE DESC-SHORTDESC         TO S2465-STATDSC.
      *
       2400-CONT.
      *
      **** IF S2465-MAILING            = SPACE                    <031>
      ****    MOVE 'Y'                 TO S2465-MAILING           <031>
      **** ELSE                                                   <031>
      **** IF S2465-MAILING            NOT = 'N' AND                    <031>
      ****    S2465-MAILING            NOT = 'Y'                        <031>
      ****      MOVE E315              TO S2465-MAILING-ERR.            <031>
      *
      **** IF S2465-DIRMAIL            = SPACE                    <031>
      ****    MOVE 'Y'                 TO S2465-DIRMAIL           <031>
      **** ELSE                                                   <031>
      ****  IF S2465-DIRMAIL           NOT = 'N' AND                    <031>
      ****     S2465-DIRMAIL           NOT = 'Y'                        <031>
      ****      MOVE E315              TO S2465-DIRMAIL-ERR.            <031>
      *
      **** IF S2465-MAILING            = 'N'  AND                       <031>
      ****    S2465-DIRMAIL            = 'Y'                            <031>
      **** IF S2465-MAILING            = SPACE AND              <A06289><031>
      ****    S2465-DIRMAIL            NOT = SPACE              <A06289><031>
      ****      MOVE F735              TO S2465-DIRMAIL-ERR.            <A06289>
      *
      **** IF S2465-VIP                = SPACE                    <031>
      ****    MOVE 'N'                 TO S2465-VIP               <031>
      **** ELSE                                                   <031>
      **** IF S2465-VIP                NOT = 'Y' AND                    <031>
      ****    S2465-VIP                NOT = 'N'                        <031>
      ****     MOVE E315               TO S2465-VIP-ERR.                <031>
      *
      *    Date of birth is not gtreater than today
      *
           IF S2465-CLTDOBX = VRCM-MAX-DATE
               GO TO 2500-CONT.
      *
           IF S2465-CLTDOBX            > WSAA-TODAY
              MOVE F073                TO S2465-CLTDOBX-ERR.
      *
      * Max Client's Age is 100:
      *
           INITIALIZE                  DTC3-DATCON3-REC.
           MOVE SPACES                 TO DTC3-FUNCTION.
           MOVE O-K                    TO DTC3-STATUZ.
           MOVE '01'                   TO DTC3-FREQUENCY.
           MOVE S2465-CLTDOBX          TO DTC3-INT-DATE-1.
           MOVE WSAA-TODAY             TO DTC3-INT-DATE-2.
           CALL 'DATCON3'              USING DTC3-DATCON3-REC.
           IF DTC3-STATUZ              NOT = O-K
               MOVE E032               TO S2465-CLTDOBX-ERR
           END-IF.
           IF DTC3-FREQ-FACTOR         > 100
               MOVE W134               TO S2465-CLTDOBX-ERR
           END-IF.
      *
           IF  S2465-ZDOCTIND          NOT = 'Y'                        <FUPLET>
           AND S2465-ZDOCTIND          NOT = 'N'                        <FUPLET>
               MOVE G616               TO S2465-ZDOCTIND-ERR            <FUPLET>
           END-IF.                                                      <FUPLET>
                                                                        <FUPLET>
       2500-CONT.
      *
      *    Date of death is validated by call to VALDODX
      *
           IF S2465-CLTDODX            = VRCM-MAX-DATE
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *        GO TO 2900-EXIT.                                         <S9503>
      ****     GO TO 2090-EXIT.                                 <PSE30> <S9503>
               GO TO 2000-CONT.                                         <PSE30>
      *
           IF  S2465-CLTDODX             > WSAA-TODAY
               MOVE F073                TO S2465-CLTDODX-ERR
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *        GO TO 2900-EXIT.                                         <S9503>
      ****     GO TO 2090-EXIT.                                 <PSE30> <S9503>
               GO TO 2000-CONT.                                         <PSE30>
      *
           IF  S2465-CLTDODX            < S2465-CLTDOBX
           AND S2465-CLTDOBX        NOT = VRCM-MAX-DATE
               MOVE F767               TO S2465-CLTDODX-ERR.
                                                                        <V64F04>
           IF S2465-CLTSTAT             = 'DN'                          <V64F04>
           OR S2465-CLTSTAT             = 'DC'                          <V64F04>
              IF S2465-CLTDODX          = VRCM-MAX-DATE                 <V64F04>
                 MOVE E186             TO S2465-CLTDODX-ERR             <V64F04>
              END-IF                                                    <V64F04>
           END-IF.                                                      <V64F04>
      *
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *2900-EXIT.                                                       <S9503>
      *2090-EXIT.                                               <PSE30> <S9503>
       2000-CONT.                                                       <PSE30>
           IF TR393-SUBRNAME         NOT = SPACES                       <FSA566>
              INITIALIZE                CLSC-CLSCVAL-REC                <FSA566>
              MOVE 'VLDT'               TO CLSC-FUNCTION                <FSA566>
              MOVE 'P'                  TO CLSC-CLTTYPE                 <FSA566>
              MOVE TR393-TR393-REC      TO CLSC-TR393-REC               <FSA566>
              MOVE S2465-DATA-AREA      TO CLSC-SCREEN-DATA             <FSA566>
              CALL TR393-SUBRNAME    USING CLSC-CLSCVAL-REC             <FSA566>
              MOVE CLSC-SCREEN-DATA     TO S2465-DATA-AREA              <FSA566>
           END-IF.                                                      <FSA566>
                                                                        <V64F04>
       2080-CHECK-ERROR.                                                <V64F04>
                                                                        <FSA566>
           IF S2465-ERROR-INDICATORS   NOT = SPACES
               MOVE 'Y'                TO WSSP-EDTERROR.
      *
      *    If the country code has changed, re-validate the screen      <A05912>
                                                                        <A05912>
           IF S2465-CTRYCODE           NOT = WSAA-CLTS-CTRYCODE         <A05912>
               MOVE S2465-CTRYCODE         TO WSAA-CLTS-CTRYCODE        <A05912>
               MOVE 'Y'                    TO WSSP-EDTERROR             <A05912>
               MOVE 'Y'                    TO WSAA-COUNTRY              <A05912>
           END-IF.                                                      <A05912>
      *                                                                 <A05912>
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *2999-EXIT.                                                       <S9503>
       2090-EXIT.                                                       <S9503>
            EXIT.
      /
                                                                        <R96REA>
      ******************************                            <R96REA>
       2200-CHECK-FOR-RACR SECTION.                                     <R96REA>
      ******************************                            <R96REA>
       2201-START.                                                      <R96REA>
                                                                        <R96REA>
           CALL 'RACRIO'               USING RACR-PARAMS.               <R96REA>
                                                                        <R96REA>
           IF RACR-STATUZ              NOT = O-K AND ENDP               <R96REA>
              MOVE RACR-PARAMS         TO RACR-PARAMS                   <R96REA>
              PERFORM 600-FATAL-ERROR                                   <R96REA>
           END-IF.                                                      <R96REA>
                                                                        <R96REA>
           IF RACR-CLNTCOY          NOT = WSSP-FSUCO   OR               <R96REA>
              RACR-CLNTPFX          NOT = CLTS-CLNTPFX OR               <R96REA>
              RACR-CLNTNUM          NOT = CLTS-CLNTNUM OR               <R96REA>
              RACR-STATUZ               = ENDP                          <R96REA>
                MOVE ENDP              TO RACR-STATUZ                   <R96REA>
                GO TO 2299-EXIT                                         <R96REA>
           END-IF.                                                      <R96REA>
                                                                        <R96REA>
      ****                                                      <R96REA>
      **** We only need to know if ANY RACRs exist, we are not  <R96REA>
      **** interested in how many. As a result, once one is foun<R96REA>
      **** there is no point proceeding.                        <R96REA>
      ****                                                      <R96REA>
                                                                        <R96REA>
           MOVE '+'                    TO S2465-RACRIND.                <R96REA>
           MOVE ENDP                   TO RACR-STATUZ.                  <R96REA>
           MOVE 'Y'                    TO WSAA-RACR-EXISTS.             <PSE309>
                                                                        <R96REA>
      **** MOVE NEXTR                  TO RACR-FUNCTION.        <R96REA><PSE309>
                                                                        <R96REA>
       2299-EXIT.                                                       <R96REA>
            EXIT.                                                       <R96REA>
      *                                                         <R96REA>
       2300-SHOW-ADDRESS-LINES SECTION.                                 <GAPPH1>
      *********************************                                 <GAPPH1>
       2301-START.                                                      <GAPPH1>
      *                                                                 <GAPPH1>
           IF WSSP-FLAG                = 'A'                            <GAPPH1>
           OR                          = 'M'                            <GAPPH1>
               IF S2465-CLTADDR-03     = SPACES                         <GAPPH1>
               AND S2465-ZPTWARD       NOT = SPACES                     <GAPPH1>
                   MOVE S2465-ZPTWARD  TO WSAA-TABLEITEM                <GAPPH1>
                   MOVE TV012          TO WSAA-TABLENAME                <GAPPH1>
                   PERFORM 2320-READ-DESC                               <GAPPH1>
                   MOVE DESC-LONGDESC  TO S2465-CLTADDR-03              <GAPPH1>
               END-IF                                                   <GAPPH1>
                                                                        <GAPPH1>
               IF S2465-CLTADDR-04     = SPACES                         <GAPPH1>
               AND S2465-ZPTDIST       NOT = SPACES                     <GAPPH1>
                   MOVE S2465-ZPTDIST  TO WSAA-TABLEITEM                <GAPPH1>
                   MOVE TV011          TO WSAA-TABLENAME                <GAPPH1>
                   PERFORM 2320-READ-DESC                               <GAPPH1>
                   MOVE DESC-LONGDESC  TO S2465-CLTADDR-04              <GAPPH1>
               END-IF                                                   <GAPPH1>
                                                                        <GAPPH1>
               IF S2465-CLTADDR-05     = SPACES                         <GAPPH1>
               AND S2465-ZPTCITY       NOT = SPACES                     <GAPPH1>
                   MOVE S2465-ZPTCITY  TO WSAA-TABLEITEM                <GAPPH1>
                   MOVE TV010          TO WSAA-TABLENAME                <GAPPH1>
                   PERFORM 2320-READ-DESC                               <GAPPH1>
                   MOVE DESC-LONGDESC  TO S2465-CLTADDR-05              <GAPPH1>
               END-IF                                                   <GAPPH1>
           END-IF.                                                      <GAPPH1>
      *                                                                 <GAPPH1>
       2319-EXIT.                                                       <GAPPH1>
           EXIT.                                                        <GAPPH1>
      /                                                                 <GAPPH1>
       2320-READ-DESC SECTION.                                          <GAPPH1>
      ************************                                          <GAPPH1>
       2321-START.                                                      <GAPPH1>
      *                                                                 <GAPPH1>
           INITIALIZE                  DESC-DATA-KEY.                   <GAPPH1>
           MOVE 'IT'                   TO DESC-DESCPFX.                 <GAPPH1>
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.                 <GAPPH1>
           MOVE WSAA-TABLENAME         TO DESC-DESCTABL.                <GAPPH1>
           MOVE WSAA-TABLEITEM         TO DESC-DESCITEM.                <GAPPH1>
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.                <GAPPH1>
           MOVE 'READR'                TO DESC-FUNCTION.                <GAPPH1>
                                                                        <GAPPH1>
           CALL 'DESCIO' USING DESC-PARAMS.                             <GAPPH1>
                                                                        <GAPPH1>
           IF DESC-STATUZ              NOT = O-K                        <GAPPH1>
           AND                         NOT = MRNF                       <GAPPH1>
               MOVE DESC-PARAMS        TO SYSR-PARAMS                   <GAPPH1>
               PERFORM 600-FATAL-ERROR                                  <GAPPH1>
           END-IF.                                                      <GAPPH1>
                                                                        <GAPPH1>
           IF DESC-STATUZ = MRNF                                        <GAPPH1>
               MOVE ALL '?'            TO DESC-LONGDESC                 <GAPPH1>
                                          DESC-SHORTDESC                <GAPPH1>
           END-IF.                                                      <GAPPH1>
      *                                                                 <GAPPH1>
       2329-EXIT.                                                       <GAPPH1>
           EXIT.                                                        <GAPPH1>
      /                                                                 <R96REA>
                                                                        <DA002>
      *****************************                                     <DA002>
                                                                        <NB008>
       2600-READ-CLEXMBP SECTION.                                       <NB008>
      ************************                                          <NB008>
       2601-START.                                                      <NB008>
      *                                                                 <NB008>
           IF WSAA-MB-NUM              NOT = S2465-RMBLPHONE            <NB008>
              MOVE SPACES              TO WSAA-MB-WARNING               <NB008>
           END-IF.                                                      <NB008>
                                                                        <NB008>
           INITIALIZE                  CLEXMBP-PARAMS.                  <NB008>
           MOVE S2465-RMBLPHONE        TO CLEXMBP-RMBLPHONE.            <NB008>
           MOVE CLEXMBPREC             TO CLEXMBP-FORMAT.               <NB008>
           MOVE BEGN                   TO CLEXMBP-FUNCTION.             <NB008>
                                                                        <NB008>
       2605-LOOP.                                                       <NB008>
           MOVE SPACES                 TO WSAA-CLNTNUM.                 <NB008>
                                                                        <NB008>
           CALL 'CLEXMBPIO'            USING CLEXMBP-PARAMS.            <NB008>
                                                                        <NB008>
           IF CLEXMBP-STATUZ           NOT = O-K                        <NB008>
           AND                         NOT = ENDP                       <NB008>
              MOVE CLEXMBP-PARAMS      TO SYSR-PARAMS                   <NB008>
              PERFORM 600-FATAL-ERROR                                   <NB008>
           END-IF.                                                      <NB008>
                                                                        <NB008>
           IF CLEXMBP-STATUZ           = ENDP                           <NB008>
           OR CLEXMBP-RMBLPHONE        NOT = S2465-RMBLPHONE            <NB008>
               GO TO 2609-EXIT                                          <NB008>
           END-IF.                                                      <NB008>
                                                                        <NB008>
           IF CLEXMBP-STATUZ           = O-K                            <NB008>
           AND CLEXMBP-RMBLPHONE       = S2465-RMBLPHONE                <NB008>
              MOVE CLEXMBP-CLNTNUM     TO WSAA-CLNTNUM                  <NB008>
              PERFORM A2600-CHECK-ID-SECURITY                           <NB008>
                                                                        <NB008>
              IF WSAA-SECURI-FND       = 'Y'                            <NB008>
                 GO TO 2608-NEXT                                        <NB008>
              END-IF                                                    <NB008>
                                                                        <NB008>
              PERFORM E2600-READ-CLTS                                   <NB008>
      *       PERFORM B2600-READ-CLRR                                   <NB008>
                                                                        <NB008>
              IF WSAA-FLAG-EXIST       = 'Y'                            <NB008>
              AND WSAA-MB-WARNING      NOT = 'Y'                        <NB008>
                 MOVE EV91             TO S2465-RMBLPHONE-ERR           <NB008>
                 MOVE 'Y'              TO WSAA-MB-WARNING               <NB008>
                 MOVE S2465-RMBLPHONE  TO WSAA-MB-NUM                   <NB008>
                 GO TO 2609-EXIT                                        <NB008>
              END-IF                                                    <NB008>
           END-IF.                                                      <NB008>
                                                                        <NB008>
       2608-NEXT.                                                       <NB008>
           MOVE NEXTR                  TO CLEXMBP-FUNCTION.             <NB008>
           GO TO 2605-LOOP.                                             <NB008>
      *                                                                 <NB008>
       2609-EXIT.                                                       <NB008>
            EXIT.                                                       <NB008>
      /                                                                 <NB008>
       A2600-CHECK-ID-SECURITY SECTION.                                 <NB008>
      *********************************                                 <NB008>
       A2601-START.                                                     <NB008>
      *                                                                 <NB008>
            MOVE 'N'                   TO WSAA-SECURI-FND.              <NB008>
                                                                        <NB008>
            INITIALIZE                 CLNTSSN-PARAMS.                  <NB008>
            MOVE S2465-SECUITYNO       TO CLNTSSN-SECUITYNO.            <NB008>
            MOVE CLNTSSNREC            TO CLNTSSN-FORMAT.               <NB008>
            MOVE BEGN                  TO CLNTSSN-FUNCTION.             <NB008>
                                                                        <NB008>
       A2605-LOOP.                                                      <NB008>
            CALL 'CLNTSSNIO'           USING CLNTSSN-PARAMS.            <NB008>
                                                                        <NB008>
            IF CLNTSSN-STATUZ          NOT = O-K                        <NB008>
            AND                        NOT = ENDP                       <NB008>
                MOVE CLNTSSN-PARAMS    TO SYSR-PARAMS                   <NB008>
                PERFORM 600-FATAL-ERROR                                 <NB008>
            END-IF.                                                     <NB008>
                                                                        <NB008>
            IF CLNTSSN-STATUZ          = ENDP                           <NB008>
            OR CLNTSSN-SECUITYNO       NOT = S2465-SECUITYNO            <NB008>
               GO TO A2609-EXIT                                         <NB008>
            END-IF.                                                     <NB008>
                                                                        <NB008>
            IF CLNTSSN-STATUZ          = O-K                            <NB008>
            AND CLNTSSN-CLNTNUM        = WSAA-CLNTNUM                   <NB008>
            AND CLNTSSN-CLTIND         = 'C'                            <NB008>
               MOVE 'Y'                TO WSAA-SECURI-FND               <NB008>
               GO TO A2609-EXIT                                         <NB008>
            END-IF.                                                     <NB008>
                                                                        <NB008>
       2608-NEXT.                                                       <NB008>
           MOVE NEXTR                  TO CLNTSSN-FUNCTION.             <NB008>
           GO TO A2605-LOOP.                                            <NB008>
      *                                                                 <NB008>
       A2609-EXIT.                                                      <NB008>
            EXIT.                                                       <NB008>
      /                                                                 <NB008>
      *B2600-READ-CLRR SECTION.                                         <NB008>
      *********************************                                 <NB008>
      *B2601-START.                                                     <NB008>
      *                                                                 <NB008>
      *     INITIALIZE                 CLRR-PARAMS.                     <NB008>
      *     MOVE 'CN'                  TO CLRR-CLNTPFX.                 <NB008>
      *     MOVE '9'                   TO CLRR-CLNTCOY.                 <NB008>
      *     MOVE WSAA-CLNTNUM          TO CLRR-CLNTNUM.                 <NB008>
      *     MOVE CLRRREC               TO CLRR-FORMAT.                  <NB008>
      *     MOVE BEGN                  TO CLRR-FUNCTION.                <NB008>
      *                                                                 <NB008>
      *B2605-LOOP.                                                      <NB008>
      *                                                                 <NB008>
      *     CALL 'CLRRIO'              USING CLRR-PARAMS.               <NB008>
      *                                                                 <NB008>
      *     IF CLRR-STATUZ             NOT = O-K                        <NB008>
      *     AND                        NOT = ENDP                       <NB008>
      *         MOVE CLRR-PARAMS       TO SYSR-PARAMS                   <NB008>
      *         PERFORM 600-FATAL-ERROR                                 <NB008>
      *     END-IF.                                                     <NB008>
      *                                                                 <NB008>
      *     IF CLRR-STATUZ             = ENDP                           <NB008>
      *     OR CLRR-CLNTNUM            NOT = WSAA-CLNTNUM               <NB008>
      *        GO TO B2609-EXIT                                         <NB008>
      *     END-IF.                                                     <NB008>
      *                                                                 <NB008>
      *     IF CLRR-STATUZ             = O-K                            <NB008>
      *     AND CLRR-CLNTNUM           = WSAA-CLNTNUM                   <NB008>
      *         IF CLRR-CLRRROLE       = 'OW'                           <NB008>
      *         OR CLRR-CLRRROLE       = 'DA'                           <NB008>
      *            MOVE 'Y'            TO WSAA-FLAG-EXIST               <NB008>
      *            GO TO B2609-EXIT                                     <NB008>
      *         END-IF                                                  <NB008>
      *     END-IF.                                                     <NB008>
      *                                                                 <NB008>
      *     MOVE 'N'            TO WSAA-FLAG-EXIST.                     <NB008>
      *                                                                 <NB008>
      *B2608-NEXT.                                                      <NB008>
      *     MOVE NEXTR          TO CLRR-FUNCTION.                       <NB008>
      *     GO TO B2605-LOOP.                                           <NB008>
      *                                                                 <NB008>
      *B2609-EXIT.                                                      <NB008>
      *     EXIT.                                                       <NB008>
      /                                                                 <NB008>
       C2600-READ-CLNTOFP SECTION.                                      <NB008>
      *********************************                                 <NB008>
       C2601-START.                                                     <NB008>
      *                                                                 <NB008>
            IF WSAA-OF-NUM             NOT = S2465-CLTPHONE-02          <NB008>
               MOVE SPACES             TO WSAA-OF-WARNING               <NB008>
            END-IF.                                                     <NB008>
                                                                        <NB008>
            INITIALIZE                 CLNTOFP-PARAMS.                  <NB008>
            MOVE S2465-CLTPHONE-02     TO CLNTOFP-CLTPHONE02.           <NB008>
            MOVE CLNTOFPREC            TO CLNTOFP-FORMAT.               <NB008>
            MOVE BEGN                  TO CLNTOFP-FUNCTION.             <NB008>
                                                                        <NB008>
       C2605-LOOP.                                                      <NB008>
            MOVE SPACES                TO WSAA-CLNTNUM.                 <NB008>
                                                                        <NB008>
            CALL 'CLNTOFPIO'           USING CLNTOFP-PARAMS.            <NB008>
                                                                        <NB008>
            IF CLNTOFP-STATUZ          NOT = O-K                        <NB008>
            AND                        NOT = ENDP                       <NB008>
                MOVE CLNTOFP-PARAMS    TO SYSR-PARAMS                   <NB008>
                PERFORM 600-FATAL-ERROR                                 <NB008>
            END-IF.                                                     <NB008>
                                                                        <NB008>
           IF CLNTOFP-STATUZ           = ENDP                           <NB008>
           OR CLNTOFP-CLTPHONE02       NOT = S2465-CLTPHONE-02          <NB008>
               GO TO C2609-EXIT                                         <NB008>
           END-IF.                                                      <NB008>
                                                                        <NB008>
           IF CLNTOFP-STATUZ           = O-K                            <NB008>
           AND CLNTOFP-CLTPHONE02      = S2465-CLTPHONE-02              <NB008>
              MOVE CLNTOFP-CLNTNUM     TO WSAA-CLNTNUM                  <NB008>
              PERFORM A2600-CHECK-ID-SECURITY                           <NB008>
                                                                        <NB008>
              IF WSAA-SECURI-FND       = 'Y'                            <NB008>
                 GO TO C2608-NEXT                                       <NB008>
              END-IF                                                    <NB008>
                                                                        <NB008>
              PERFORM E2600-READ-CLTS                                   <NB008>
      *       PERFORM B2600-READ-CLRR                                   <NB008>
                                                                        <NB008>
              IF WSAA-FLAG-EXIST       = 'Y'                            <NB008>
              AND WSAA-OF-WARNING      NOT = 'Y'                        <NB008>
                 MOVE EV92             TO S2465-CLTPHONE02-ERR          <NB008>
                 MOVE 'Y'              TO WSAA-OF-WARNING               <NB008>
                 MOVE S2465-CLTPHONE-02 TO WSAA-OF-NUM                  <NB008>
                 GO TO C2609-EXIT                                       <NB008>
              END-IF                                                    <NB008>
           END-IF.                                                      <NB008>
                                                                        <NB008>
       C2608-NEXT.                                                      <NB008>
           MOVE NEXTR                  TO CLNTOFP-FUNCTION.             <NB008>
           GO TO C2605-LOOP.                                            <NB008>
      *                                                                 <NB008>
       C2609-EXIT.                                                      <NB008>
            EXIT.                                                       <NB008>
      /                                                                 <NB008>
       D2600-READ-CLNTHSP SECTION.                                      <NB008>
      *********************************                                 <NB008>
       D2601-START.                                                     <NB008>
      *                                                                 <NB008>
            IF WSAA-HS-NUM             NOT = S2465-CLTPHONE-01          <NB008>
               MOVE SPACES             TO  WSAA-HS-WARNING              <NB008>
            END-IF.                                                     <NB008>
                                                                        <NB008>
            INITIALIZE                 CLNTHSP-PARAMS.                  <NB008>
            MOVE S2465-CLTPHONE-01     TO CLNTHSP-CLTPHONE01.           <NB008>
            MOVE CLNTHSPREC            TO CLNTHSP-FORMAT.               <NB008>
            MOVE BEGN                  TO CLNTHSP-FUNCTION.             <NB008>
                                                                        <NB008>
       D2605-LOOP.                                                      <NB008>
            MOVE SPACES                 TO WSAA-CLNTNUM.                <NB008>
                                                                        <NB008>
            CALL 'CLNTHSPIO'           USING CLNTHSP-PARAMS.            <NB008>
                                                                        <NB008>
            IF CLNTHSP-STATUZ          NOT = O-K                        <NB008>
            AND                        NOT = ENDP                       <NB008>
                MOVE CLNTHSP-PARAMS    TO SYSR-PARAMS                   <NB008>
                PERFORM 600-FATAL-ERROR                                 <NB008>
            END-IF.                                                     <NB008>
                                                                        <NB008>
           IF CLNTHSP-STATUZ           = ENDP                           <NB008>
           OR CLNTHSP-CLTPHONE01       NOT = S2465-CLTPHONE-01          <NB008>
               GO TO D2609-EXIT                                         <NB008>
           END-IF.                                                      <NB008>
                                                                        <NB008>
           IF CLNTHSP-STATUZ           = O-K                            <NB008>
           AND CLNTHSP-CLTPHONE01      = S2465-CLTPHONE-01              <NB008>
              MOVE CLNTHSP-CLNTNUM     TO WSAA-CLNTNUM                  <NB008>
              PERFORM A2600-CHECK-ID-SECURITY                           <NB008>
                                                                        <NB008>
              IF WSAA-SECURI-FND       = 'Y'                            <NB008>
                 GO TO D2608-NEXT                                       <NB008>
              END-IF                                                    <NB008>
                                                                        <NB008>
              PERFORM E2600-READ-CLTS                                   <NB008>
      *       PERFORM B2600-READ-CLRR                                   <NB008>
                                                                        <NB008>
              IF  WSAA-FLAG-EXIST      = 'Y'                            <NB008>
              AND WSAA-HS-WARNING      NOT = 'Y'                        <NB008>
                 MOVE EV93             TO S2465-CLTPHONE01-ERR          <NB008>
                 MOVE 'Y'              TO WSAA-HS-WARNING               <NB008>
                 MOVE S2465-CLTPHONE-01     TO WSAA-HS-NUM              <NB008>
                 GO TO D2609-EXIT                                       <NB008>
              END-IF                                                    <NB008>
           END-IF.                                                      <NB008>
                                                                        <NB008>
       D2608-NEXT.                                                      <NB008>
           MOVE NEXTR                  TO CLNTHSP-FUNCTION.             <NB008>
           GO TO D2605-LOOP.                                            <NB008>
      *                                                                 <NB008>
       D2609-EXIT.                                                      <NB008>
            EXIT.                                                       <NB008>
      /                                                                 <NB008>
       E2600-READ-CLTS SECTION.                                         <NB008>
      *********************************                                 <NB008>
       E2601-START.                                                     <NB008>
                                                                        <NB008>
            INITIALIZE                 CLTS-PARAMS.                     <NB008>
            MOVE 'CN'                  TO CLTS-CLNTPFX.                 <NB008>
            MOVE '9'                   TO CLTS-CLNTCOY.                 <NB008>
            MOVE WSAA-CLNTNUM          TO CLTS-CLNTNUM.                 <NB008>
            MOVE CLTSREC               TO CLTS-FORMAT.                  <NB008>
            MOVE BEGN                  TO CLTS-FUNCTION.                <NB008>
                                                                        <NB008>
       E2605-LOOP.                                                      <NB008>
                                                                        <NB008>
            CALL 'CLTSIO'              USING CLTS-PARAMS.               <NB008>
                                                                        <NB008>
            IF CLTS-STATUZ             NOT = O-K                        <NB008>
            AND                        NOT = ENDP                       <NB008>
                MOVE CLTS-PARAMS       TO SYSR-PARAMS                   <NB008>
                PERFORM 600-FATAL-ERROR                                 <NB008>
            END-IF.                                                     <NB008>
                                                                        <NB008>
            IF CLTS-STATUZ             = ENDP                           <NB008>
            OR CLTS-CLNTNUM            NOT = WSAA-CLNTNUM               <NB008>
               GO TO E2609-EXIT                                         <NB008>
            END-IF.                                                     <NB008>
                                                                        <NB008>
            IF CLTS-STATUZ             = O-K                            <NB008>
            AND CLTS-CLNTNUM           = WSAA-CLNTNUM                   <NB008>
                IF CLTS-CLTIND         NOT = 'D'                        <NB008>
                   MOVE 'Y'            TO WSAA-FLAG-EXIST               <NB008>
                   GO TO E2609-EXIT                                     <NB008>
                END-IF                                                  <NB008>
            END-IF.                                                     <NB008>
                                                                        <NB008>
            MOVE 'N'            TO WSAA-FLAG-EXIST.                     <NB008>
                                                                        <NB008>
       E2608-NEXT.                                                      <NB008>
            MOVE NEXTR          TO CLTS-FUNCTION.                       <NB008>
            GO TO E2605-LOOP.                                           <NB008>
                                                                        <NB008>
       E2609-EXIT.                                                      <NB008>
            EXIT.                                                       <NB008>
      /                                                                 <NB008>
       2700-READ-TV087 SECTION.                                         <DA002>
      ************************                                          <DA002>
       2701-START.                                                      <DA002>
      ************************                                          <DA002>
           INITIALIZE                  ITEM-PARAMS.                     <DA002>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <DA002>
           MOVE '2'                    TO ITEM-ITEMCOY.                 <DA002>
           MOVE TV087                  TO ITEM-ITEMTABL.                <DA002>
           MOVE WSSP-USERID            TO ITEM-ITEMITEM.                <DA002>
           MOVE SPACES                 TO ITEM-ITEMSEQ.                 <DA002>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <DA002>
           MOVE READR                  TO ITEM-FUNCTION.                <DA002>
                                                                        <DA002>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <DA002>
                                                                        <DA002>
           IF ITEM-STATUZ              NOT = O-K                        <DA002>
           AND                         NOT = MRNF                       <DA002>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <DA002>
              PERFORM 600-FATAL-ERROR                                   <DA002>
           END-IF.                                                      <DA002>
                                                                        <DA002>
           IF ITEM-STATUZ              = MRNF                           <DA002>
              GO TO 2709-EXIT                                           <DA002>
           END-IF.                                                      <DA002>
                                                                        <DA002>
           IF ITEM-STATUZ              = O-K                            <DA002>
           AND S2465-SECUITYNO         NOT = WSAA-FIR-SECUITYNO         <DA002>
               MOVE P129               TO S2465-SECUITYNO-ERR           <DA002>
           END-IF.                                                      <DA002>
                                                                        <DA002>
      ************************                                          <DA002>
       2709-EXIT.                                                       <DA002>
           EXIT.                                                        <DA002>
      ************************                                          <DA002>
                                                                        <DA002>
       2800-READ-CLRRWIN SECTION.                                       <DA002>
      ************************                                          <DA002>
       2801-START.                                                      <DA002>
      ************************                                          <DA002>
           INITIALIZE                  CLRRWIN-PARAMS.                  <DA002>
           MOVE WSSP-COMPANY           TO CLRRWIN-FORECOY.              <DA002>
           MOVE S2465-CLNTNUM          TO CLRRWIN-CLNTNUM.              <DA002>
           MOVE 'AG'                   TO CLRRWIN-CLRRROLE.             <DA002>
           MOVE CLRRWINREC             TO CLRRWIN-FORMAT.               <DA002>
           MOVE READR                  TO CLRRWIN-FUNCTION.             <DA002>
                                                                        <DA002>
           CALL 'CLRRWINIO'            USING CLRRWIN-PARAMS.            <DA002>
                                                                        <DA002>
           IF CLRRWIN-STATUZ           NOT = O-K                        <DA002>
           AND                         NOT = MRNF                       <DA002>
               MOVE CLRRWIN-PARAMS     TO SYSR-PARAMS                   <DA002>
               PERFORM 600-FATAL-ERROR                                  <DA002>
           END-IF.                                                      <DA002>
                                                                        <DA002>
           IF CLRRWIN-STATUZ           = MRNF                           <DA002>
              GO TO 2809-EXIT                                           <DA002>
           END-IF.                                                      <DA002>
                                                                        <DA002>
           IF CLRRWIN-STATUZ           = O-K                            <DA002>
           AND CLRRWIN-CLNTNUM         = S2465-CLNTNUM                  <DA002>
           AND CLRRWIN-CLRRROLE        = 'AG'                           <DA002>
              PERFORM 2700-READ-TV087                                   <DA002>
           END-IF.                                                      <DA002>
                                                                        <DA002>
      ************************                                          <DA002>
       2809-EXIT.                                                       <DA002>
           EXIT.                                                        <DA002>
      /                                                                 <NB008>
      *****************************************************************
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3200-LOAD-RREC.
      *
      *   Skip screen if CF11 pressed - kill.
      *
      *    IF SCRN-STATUZ = KILL                                        <FA5288>
      *                  OR 'DIRY'                                      <FA5288>
           IF SCRN-STATUZ = 'DIRY'                                      <FA5288>
                         OR 'SERV'
             OR WSSP-FLAG = 'I'
             OR WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
               GO TO R201-EXIT.
      *                                                                 <FA5288>
           IF SCRN-STATUZ              = KILL                           <FA5288>
              IF  WSSP-FLAG            = 'A'                            <FA5288>
              AND WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) = SPACE               <FA5288>
                 PERFORM 3100-DELETE-CLNT                               <FA5288>
                 PERFORM 3100-DELETE-CLTS                               <FA5288>
                 PERFORM A5600-RETN-ALOCNO                              <FA5288>
              END-IF                                                    <FA5288>
              GO TO R201-EXIT                                           <FA5288>
           END-IF.                                                      <FA5288>
      *
       001-WRITE-RECORD.
           IF WSSP-FLAG NOT             = 'A' AND                       <V64F04>
              WSSP-SBMACTION        NOT = 'J'                           <V64F04>
               GO TO 002-DELET-RECORD                                   <V64F04>
           END-IF.                                                      <V64F04>
                                                                        <V73F02>
      ***  IF  WSSP-SBMACTION           = 'J'                   <V76F10><V73F02>
      ***      MOVE '2'                TO CLTS-VALIDFLAG        <V76F10><V73F02>
      ***      MOVE WRITD              TO CLTS-FUNCTION         <V76F10><V73F02>
      ***      CALL 'CLTSIO'        USING CLTS-PARAMS           <V76F10><V73F02>
      ***      IF  CLTS-STATUZ      NOT = O-K                   <V76F10><V73F02>
      ***          MOVE CLTS-PARAMS    TO SYSR-PARAMS           <V76F10><V73F02>
      ***          PERFORM 600-FATAL-ERROR                      <V76F10><V73F02>
      ***      END-IF                                           <V76F10><V73F02>
      ***  END-IF.                                              <V76F10><V73F02>
                                                                        <V73F02>
           MOVE WSSP-TRANID            TO VRCM-TRANID.
           MOVE VRCM-TRANID            TO CLTS-TRANID.
           MOVE WSSP-CLNTKEY           TO CLTS-DATA-KEY.
      **** MOVE S2465-SURNAME          TO CLTS-SURNAME.                 <PSE30>
      **** MOVE S2465-GIVNAME          TO CLTS-GIVNAME.                 <PSE30>
           IF T3711-FLAG = 'Y'                                          <PSE30>
              MOVE S2465-LSURNAME      TO CLTS-GIVNAME                  <PSE30>
              MOVE S2465-LGIVNAME      TO CLTS-SURNAME                  <PSE30>
           ELSE                                                         <PSE30>
              MOVE S2465-LSURNAME      TO CLTS-SURNAME                  <PSE30>
              MOVE S2465-LGIVNAME      TO CLTS-GIVNAME                  <PSE30>
           END-IF.                                                      <PSE30>
           MOVE S2465-LSURNAME         TO CLTS-LSURNAME.                <PSE30>
           MOVE S2465-LGIVNAME         TO CLTS-LGIVNAME.                <PSE30>
      **** MOVE S2465-SALUT            TO CLTS-SALUTL.          <WLI.P2><CAS2.0>
           MOVE S2465-SALUTL           TO CLTS-SALUTL.                  <CAS2.0>
      **** MOVE S2465-MIDDL-01         TO CLTS-MIDDL01.                 <PSE30>
      **** MOVE S2465-MIDDL-02         TO CLTS-MIDDL02.                 <PSE30>
           MOVE SPACES                 TO CLTS-MIDDL01                  <PSE30>
                                          CLTS-MIDDL02.                 <PSE30>
      **** MOVE S2465-GIVNAME          TO WSAA-INT1.                    <PSE30>
           MOVE S2465-LGIVNAME         TO WSAA-INT1.                    <PSE30>
      **** MOVE S2465-MIDDL-01         TO WSAA-INT2.                    <PSE30>
      **** MOVE S2465-MIDDL-02         TO WSAA-INT3.                    <PSE30>
           MOVE SPACES                 TO WSAA-INT2                     <PSE30>
                                          WSAA-INT3.                    <PSE30>
           MOVE WSAA-INTLMD            TO CLTS-INITIALS.
           IF T1680-FUNCKEYPR      NOT = '1'                            <MLS001>
            AND T1680-LANGUAGE-DBCS    = 'Y'                            <MLS001>
                MOVE SPACES            TO CLTS-INITIALS                 <MLS001>
           END-IF.                                                      <MLS001>
           MOVE S2465-MARRYD           TO CLTS-MARRYD.
           MOVE S2465-BIRTHP           TO CLTS-BIRTHP.
           MOVE S2465-CLTSEX           TO CLTS-CLTSEX.
           MOVE S2465-CLTADDR-01       TO CLTS-CLTADDR01.
           MOVE S2465-CLTADDR-02       TO CLTS-CLTADDR02.
           MOVE S2465-CLTADDR-03       TO CLTS-CLTADDR03.
           MOVE S2465-CLTADDR-04       TO CLTS-CLTADDR04.
           MOVE S2465-CLTADDR-05       TO CLTS-CLTADDR05.
      **** MOVE S2465-CLTPCODE         TO CLTS-CLTPCODE.                <GAPPH1>
           MOVE S2465-ZPTCITY          TO WSAA-CITY.                    <GAPPH1>
           MOVE S2465-ZPTDIST          TO WSAA-DIST.                    <GAPPH1>
           MOVE S2465-ZPTWARD          TO WSAA-WARD.                    <GAPPH1>
           MOVE WSAA-LOCCODE           TO CLTS-CLTPCODE.                <GAPPH1>
           MOVE S2465-CTRYCODE         TO CLTS-CTRYCODE.
           MOVE S2465-NATLTY           TO CLTS-NATLTY.
           MOVE S2465-MAILING          TO CLTS-MAILING.
           MOVE S2465-DIRMAIL          TO CLTS-DIRMAIL.
           MOVE S2465-ADDRTYPE         TO CLTS-ADDRTYPE.
           MOVE S2465-CLTPHONE-01      TO CLTS-CLTPHONE01.
           MOVE S2465-CLTPHONE-02      TO CLTS-CLTPHONE02.
           MOVE S2465-SERVBRH          TO CLTS-SERVBRH.
           MOVE S2465-STATCODE         TO CLTS-STATCODE.
           MOVE S2465-SECUITYNO        TO CLTS-SECUITYNO.
           MOVE S2465-OCCPCODE         TO CLTS-OCCPCODE.
           MOVE S2465-VIP              TO CLTS-VIP.
           MOVE S2465-CLTDOBX          TO CLTS-CLTDOB.
           MOVE S2465-CLTDODX          TO CLTS-CLTDOD.
           MOVE S2465-SOE              TO CLTS-SOE.
           MOVE S2465-DOCNO            TO CLTS-DOCNO.
                                                                        <V64F04>
      **** IF  S2465-CLTDODX NOT = VRCM-MAX-DATE                        <V64F04>
      ****     MOVE 'DN'               TO CLTS-CLTSTAT                  <V64F04>
      **** ELSE                                                         <V64F04>
      ****     MOVE 'AC'               TO CLTS-CLTSTAT.                 <V64F04>
                                                                        <V64F04>
           IF S2465-CLTDODX         NOT = VRCM-MAX-DATE                 <V64F04>
              MOVE 'DN'                TO CLTS-CLTSTAT                  <V64F04>
           ELSE                                                         <V64F04>
              IF WSSP-SBMACTION         = 'A'                           <V64F04>
              OR WSSP-SBMACTION         = '8'                           <V64F04>
              OR S2465-CLTSTAT          = SPACES                        <V64F04>
                 MOVE 'AC'              TO CLTS-CLTSTAT                 <V64F04>
                 MOVE 'E'               TO CLTS-LANGUAGE                <NB006>
              ELSE                                                      <V64F04>
      ****    IF WSSP-SBMACTION         = 'J'                           <V64F04>
                 MOVE S2465-CLTSTAT     TO CLTS-CLTSTAT                 <V64F04>
              END-IF                                                    <V64F04>
           END-IF.                                                      <V64F04>
                                                                        <V64F04>
      *                                                                 <FA5150>
      **** MOVE READH                  TO CLTS-FUNCTION.        <FA5297><FA5150>
      **** MOVE CLTSREC                TO CLTS-FORMAT.          <FA5297><FA5150>
                                                                        <FA5150>
      **** CALL 'CLTSIO'                 USING CLTS-PARAMS      <FA5297><FA5150>
                                                                        <FA5150>
      **** IF CLTS-STATUZ              NOT = O-K AND MRNF       <FA5297><FA5150>
      ****    MOVE CLTS-PARAMS        TO SYSR-PARAMS            <FA5297><FA5150>
      ****    MOVE CLTS-STATUZ         TO SYSR-STATUZ           <FA5297><FA5150>
      ****    PERFORM 600-FATAL-ERROR                           <FA5297><FA5150>
      **** END-IF.                                              <FA5297><FA5150>
      *                                                                 <FA5150>
           MOVE ZEROS                  TO CLTS-CAPITAL.                 <025>
           MOVE S2465-START-DATE       TO CLTS-START-DATE.              <025>
      **** MOVE S2465-ETHORIG          TO CLTS-ETHORIG.         <GRP2.0><025>
           MOVE S2465-NMFMT            TO CLTS-ETHORIG.                 <GRP2.0>
           MOVE S2465-LANGUAGE         TO CLTS-LANGUAGE.                <025>
           MOVE 'P'                    TO CLTS-CLTTYPE.
           MOVE 'N'                    TO CLTS-CLTMCHG.
           MOVE 1                      TO CLTS-VALIDFLAG.
      *****MOVE 'C'                    TO CLTS-CLTIND.
           MOVE WSAA-CLTIND            TO CLTS-CLTIND.
           MOVE S2465-TAXFLAG          TO CLTS-TAXFLAG.                 <V63P31>
           MOVE UPDAT                  TO CLTS-FUNCTION.                <V76F10>
      **** MOVE UPDAT                  TO CLTS-FUNCTION.                <V73F02>
      ***  IF  WSAA-FIRST-LOOP          = 'Y'                   <V76F10><V73F02>
      ***      MOVE WRITR              TO CLTS-FUNCTION         <V76F10><V73F02>
      ***  ELSE                                                 <V76F10><V73F02>
      ***      MOVE REWRT              TO CLTS-FUNCTION         <V76F10><V73F02>
      ***  END-IF                                               <V76F10><V73F02>
           CALL 'CLTSIO'                 USING CLTS-PARAMS
           IF CLTS-STATUZ              NOT = O-K
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
                                                                        <V72F01>
      * Update CLEXPF                                                   <V72F01>
           PERFORM 3800-UPDATE-CLEX.                                    <V72F01>
      * Update ZCLEPF                                                   <CLM14>
           PERFORM A3800-UPDATE-ZCLE.                                   <CLM14>
                                                                        <GAPPH2>
      *                                                                 <GAPPH2>
      * Update Staff Client information (SCLTPF)                        <GAPPH2>
      *                                                                 <GAPPH2>
           PERFORM 3900-UPDATE-SCLT.                                    <GAPPH2>
                                                                        <GAPPH2>
      *                                                                 <ICA011>
      *  Grant access to a client                                       <ICA011>
      *                                                                 <ICA011>
      **** PERFORM A5000-SANCTION-CLIENT.                       <V75F01><ICA011>
      *                                                                 <FUPLET>
      * Create the Doctor Role if the occupation is Doctor.             <FUPLET>
      *                                                                 <FUPLET>
           IF S2465-ZDOCTIND           = 'Y'                            <FUPLET>
              MOVE SPACES              TO CLRN-CLTRELN-REC              <FUPLET>
              MOVE PRFX-CLNT           TO CLRN-CLNTPFX                  <FUPLET>
              MOVE WSSP-FSUCO          TO CLRN-CLNTCOY                  <FUPLET>
              MOVE S2465-CLNTNUM       TO CLRN-CLNTNUM                  <FUPLET>
              MOVE CLRF-DOCTOR         TO CLRN-CLRRROLE                 <FUPLET>
                                          CLRN-FOREPFX                  <FUPLET>
              MOVE WSSP-COMPANY        TO CLRN-FORECOY                  <FUPLET>
              MOVE S2465-CLNTNUM       TO CLRN-FORENUM                  <FUPLET>
              MOVE 'ADD'               TO CLRN-FUNCTION                 <FUPLET>
                                                                        <FUPLET>
              CALL 'CLTRELN'        USING CLRN-CLTRELN-REC              <FUPLET>
                                                                        <FUPLET>
              IF CLRN-STATUZ        NOT = O-K                           <FUPLET>
                 MOVE CLRN-CLTRELN-REC TO SYSR-PARAMS                   <FUPLET>
                 MOVE CLRN-STATUZ      TO SYSR-STATUZ                   <FUPLET>
                 PERFORM 600-FATAL-ERROR                                <FUPLET>
              END-IF                                                    <FUPLET>
           END-IF.                                                      <FUPLET>
                                                                        <FUPLET>
           GO TO 004-COMMIT.
      *
       002-DELET-RECORD.
           IF WSSP-FLAG NOT = 'D'
               GO TO 003-MODIFY-RECORD.
           MOVE READH                  TO CLTS-FUNCTION.
           MOVE CLTSREC                TO CLTS-FORMAT.
      *
           CALL 'CLTSIO'                 USING CLTS-PARAMS
      *
           IF CLTS-STATUZ              NOT = O-K
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
      *     Delete CLTS record if found
      *
                                                                        <A06083>
      **** MOVE DELET                  TO CLTS-FUNCTION.                <V73F02>
                                                                        <V73F02>
           MOVE '2'                    TO CLTS-VALIDFLAG.               <V73F02>
                                                                        <V73F02>
           MOVE REWRT                  TO CLTS-FUNCTION.                <V73F02>
           MOVE CLTSREC                TO CLTS-FORMAT.                  <A06083>
                                                                        <A06083>
           CALL 'CLTSIO'               USING CLTS-PARAMS                <A06083>
                                                                        <A06083>
           IF CLTS-STATUZ           NOT = O-K                           <A06083>
               MOVE CLTS-STATUZ        TO SYSR-STATUZ                   <A06083>
               MOVE CLTS-PARAMS        TO SYSR-PARAMS                   <A06083>
               PERFORM 600-FATAL-ERROR                                  <A06083>
           END-IF.                                                      <A06083>
      *  Revoke access to a client                                      <ICA011>
      *                                                                 <ICA011>
      **** PERFORM A5000-SANCTION-CLIENT.                       <V75F01><ICA011>
                                                                        <ICA011>
      *                                                                 <CN005>
      *    Delete corresponding record in the Client Extra Fields file  <CN005>
               PERFORM D1000-DELETE-CLEX.                               <CN005>
      *                                                                 <CN005>
      *    Delete corresponding record in the RACR file                 <PSE309>
               PERFORM D2000-DELETE-RACR.                               <PSE309>

      *    Delete corresponding record in the BRUP file                 <V71L02>
               PERFORM D4000-DELETE-BRUP.                               <V71L02>
                                                                        <V71L02>
      *    Delete corresponding record in the CLPR file                 <V74F03>
               PERFORM D5000-DELETE-CLPR.                               <V74F03>
      ****************************************************************
      *    Delete all Source of Income and Salary History records
      *
           MOVE CLTS-CLNTCOY           TO SALH-CLNTCOY.
           MOVE CLTS-CLNTNUM           TO SALH-CLNTNUM.
           MOVE ZEROES                 TO SALH-INCOME-SEQ-NO.
           MOVE 9999                   TO SALH-TAX-YEAR.
           MOVE SALHREC                TO SALH-FORMAT.
           MOVE BEGNH                  TO SALH-FUNCTION.
      *
       002-DELET-SALH.
      *
           CALL 'SALHIO'                 USING SALH-PARAMS.
      *
           IF SALH-STATUZ              NOT = O-K
                                   AND NOT = ENDP
               MOVE SALH-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF SALH-STATUZ                  = ENDP
              GO TO 002-DELET-SOIN-BEGIN.
      *
           IF  SALH-CLNTCOY            NOT = CLTS-CLNTCOY
           OR  SALH-CLNTNUM            NOT = CLTS-CLNTNUM
      *
      *  Read non-existant record to drop unwanted record lock
      *
              MOVE SPACES              TO SALH-DATA-KEY
              MOVE SALHREC             TO SALH-FORMAT
              MOVE READH               TO SALH-FUNCTION
              CALL 'SALHIO'            USING SALH-PARAMS
              GO TO 002-DELET-SOIN-BEGIN.
      *
      **** MOVE DELET                  TO SALH-FUNCTION.                <V73F02>
                                                                        <V73F02>
           MOVE '2'                    TO SALH-VALIDFLAG.               <V73F02>
                                                                        <V73F02>
           MOVE REWRT                  TO SALH-FUNCTION.                <V73F02>
           CALL 'SALHIO'                 USING SALH-PARAMS.
      *
           IF SALH-STATUZ              NOT = O-K
               MOVE SALH-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           MOVE NEXTR                  TO SALH-FUNCTION.
           GO TO 002-DELET-SALH.

      *
      *  Delete source of income records
      *
       002-DELET-SOIN-BEGIN.
           MOVE CLTS-CLNTCOY           TO SOIN-CLNTCOY.
           MOVE CLTS-CLNTNUM           TO SOIN-CLNTNUM.
           MOVE ZEROES                 TO SOIN-INCOME-SEQ-NO.
           MOVE BEGNH                  TO SOIN-FUNCTION.
      *
       002-DELET-SOIN.
      *
           CALL 'SOINIO'                 USING SOIN-PARAMS.
      *
           IF SOIN-STATUZ              NOT = O-K
                                   AND NOT = ENDP
               MOVE SOIN-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF SOIN-STATUZ                  = ENDP
              GO TO 002-DELET-CLRF-BEGIN.
      *
           IF  SOIN-CLNTCOY            NOT = CLTS-CLNTCOY
           OR  SOIN-CLNTNUM            NOT = CLTS-CLNTNUM
      *
      *  Read non-existant record to drop unwanted record lock
      *
              MOVE SPACES              TO SOIN-DATA-KEY
              MOVE READH               TO SOIN-FUNCTION
              CALL 'SOINIO'            USING SOIN-PARAMS
              GO TO 002-DELET-CLRF-BEGIN.
      *
      **** MOVE DELET                  TO SOIN-FUNCTION.                <V73F02>
                                                                        <V73F02>
           MOVE '2'                    TO SOIN-VALIDFLAG.               <V73F02>
                                                                        <V73F02>
           MOVE REWRT                  TO SOIN-FUNCTION.                <V73F02>
           CALL 'SOINIO'               USING SOIN-PARAMS.
      *
           IF SOIN-STATUZ              NOT = O-K
               MOVE SOIN-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           MOVE NEXTR                  TO SOIN-FUNCTION.
           GO TO 002-DELET-SOIN.


       002-DELET-CLRF-BEGIN.
                                                                        <FUPLET>
           IF S2465-ZDOCTIND           = 'Y'                            <FUPLET>
              MOVE SPACES              TO CLRN-CLTRELN-REC              <FUPLET>
              MOVE PRFX-CLNT           TO CLRN-CLNTPFX                  <FUPLET>
              MOVE WSSP-FSUCO          TO CLRN-CLNTCOY                  <FUPLET>
              MOVE S2465-CLNTNUM       TO CLRN-CLNTNUM                  <FUPLET>
              MOVE CLRF-DOCTOR         TO CLRN-CLRRROLE                 <FUPLET>
                                          CLRN-FOREPFX                  <FUPLET>
              MOVE WSSP-COMPANY        TO CLRN-FORECOY                  <FUPLET>
              MOVE S2465-CLNTNUM       TO CLRN-FORENUM                  <FUPLET>
              MOVE 'DEL'               TO CLRN-FUNCTION                 <FUPLET>
                                                                        <FUPLET>
              CALL 'CLTRELN'           USING CLRN-CLTRELN-REC           <FUPLET>
                                                                        <FUPLET>
              IF CLRN-STATUZ           NOT = O-K                        <FUPLET>
                 MOVE CLRN-CLTRELN-REC TO SYSR-PARAMS                   <FUPLET>
                 MOVE CLRN-STATUZ      TO SYSR-STATUZ                   <FUPLET>
                 PERFORM 600-FATAL-ERROR                                <FUPLET>
              END-IF                                                    <FUPLET>
           END-IF.                                                      <FUPLET>
      ****************************************************************
      *     Delete all CLRF records for foreign key
      *
           MOVE 'CN'                   TO CLRF-FOREPFX.
           MOVE CLTS-CLNTCOY           TO CLRF-FORECOY.
           MOVE CLTS-CLNTNUM           TO CLRF-FORENUM.
           MOVE SPACES                 TO CLRF-CLRRROLE.
           MOVE CLRFREC                TO CLRF-FORMAT.
           MOVE BEGNH                  TO CLRF-FUNCTION.
      *
       002-DELET-CLRF.
      *
           CALL 'CLRFIO'                 USING CLRF-PARAMS.
      *
           IF CLRF-STATUZ              NOT = O-K
                                   AND NOT = ENDP
               MOVE CLRF-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           IF CLRF-STATUZ                  = ENDP
              IF WSAA-UPDATE-CLNT          = 'Y'                        <027>
                 PERFORM 3400-BEGIN-CLRR                                <027>
                 MOVE 'N'              TO WSAA-UPDATE-CLNT              <027>
              END-IF                                                    <027>
               GO TO 004-COMMIT.
      *
           IF CLRF-FOREPFX            NOT = 'CN'
            OR CLRF-FORECOY            NOT = CLTS-CLNTCOY
            OR CLRF-FORENUM            NOT = CLTS-CLNTNUM
      *
      *  Read non-existant record to drop unwanted record lock
      *
              MOVE SPACES              TO CLRF-DATA-KEY
              MOVE CLRFREC             TO CLRF-FORMAT
              MOVE READH               TO CLRF-FUNCTION
              CALL 'CLRFIO'            USING CLRF-PARAMS
              IF WSAA-UPDATE-CLNT          = 'Y'                        <027>
                 PERFORM 3400-BEGIN-CLRR                                <027>
                 MOVE 'N'              TO WSAA-UPDATE-CLNT              <027>
              END-IF                                                    <027>
               GO TO 004-COMMIT.
      *
           IF CLRF-CLRRROLE            = 'AA' OR 'AL'                   <027>
              MOVE 'Y'                 TO WSAA-UPDATE-CLNT              <027>
              MOVE CLRF-CLNTNUM        TO WSAA-CLNTNUM                  <027>
              MOVE CLRF-CLRRROLE       TO WSAA-CLRROLE                  <027>
           END-IF.                                                      <027>
           MOVE DELET                  TO CLRF-FUNCTION.
           CALL 'CLRFIO'                 USING CLRF-PARAMS.
      *
           IF CLRF-STATUZ              NOT = O-K
               MOVE CLRF-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           MOVE NEXTR                  TO CLRF-FUNCTION.
           GO TO 002-DELET-CLRF.
      *
       003-MODIFY-RECORD.
           IF WSSP-FLAG NOT = 'M'
               GO TO 004-COMMIT.
      *                                                                 <PS1204>
      * Ensure that master Client Number is locked to prevent Client    <PS1204>
      *  roles from being o/riden by Alt.Addr or Alias client rec.      <PS1204>
      *                                                                 <PS1204>
           MOVE WSSP-CLNTKEY           TO CLTS-DATA-KEY.                <PS1204>
           MOVE READH                  TO CLTS-FUNCTION.
           MOVE CLTSREC                TO CLTS-FORMAT.
           CALL 'CLTSIO'                 USING CLTS-PARAMS
           IF CLTS-STATUZ              NOT = O-K
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
                                                                        <V73F02>
      ***  IF  WSAA-FIRST-LOOP          = 'Y'                   <V76F10><V73F02>
      ***      MOVE '2'                TO CLTS-VALIDFLAG        <V76F10><V73F02>
      ***      MOVE REWRT              TO CLTS-FUNCTION         <V76F10><V73F02>
      ***      MOVE CLTSREC            TO CLTS-FORMAT           <V76F10><V73F02>
      ***      CALL 'CLTSIO'        USING CLTS-PARAMS           <V76F10><V73F02>
      ***      IF  CLTS-STATUZ      NOT = O-K                   <V76F10><V73F02>
      ***          MOVE CLTS-PARAMS    TO SYSR-PARAMS           <V76F10><V73F02>
      ***          PERFORM 600-FATAL-ERROR                      <V76F10><V73F02>
      ***      END-IF                                           <V76F10><V73F02>
      ***  END-IF.                                              <V76F10><V73F02>
                                                                        <V73F02>
           MOVE VRCM-TRANID            TO CLTS-TRANID.
           MOVE WSSP-CLNTKEY           TO CLTS-DATA-KEY.
      **** MOVE S2465-SURNAME          TO CLTS-SURNAME.                 <PSE30>
      **** MOVE S2465-GIVNAME          TO CLTS-GIVNAME.                 <PSE30>
           IF T3711-FLAG = 'Y'                                          <PSE30>
              MOVE S2465-LSURNAME      TO CLTS-GIVNAME                  <PSE30>
              MOVE S2465-LGIVNAME      TO CLTS-SURNAME                  <PSE30>
           ELSE                                                         <PSE30>
              MOVE S2465-LSURNAME      TO CLTS-SURNAME                  <PSE30>
              MOVE S2465-LGIVNAME      TO CLTS-GIVNAME                  <PSE30>
           END-IF.                                                      <PSE30>
           MOVE S2465-LSURNAME         TO CLTS-LSURNAME.                <PSE30>
           MOVE S2465-LGIVNAME         TO CLTS-LGIVNAME.                <PSE30>
      **** MOVE S2465-SALUT            TO CLTS-SALUTL.          <WLI.P2><CAS2.0>
           MOVE S2465-SALUTL           TO CLTS-SALUTL.                  <CAS2.0>
      **** MOVE S2465-MIDDL-01         TO CLTS-MIDDL01.                 <PSE30>
      **** MOVE S2465-MIDDL-02         TO CLTS-MIDDL02.                 <PSE30>
           MOVE SPACES                 TO CLTS-MIDDL01                  <PSE30>
                                          CLTS-MIDDL02.                 <PSE30>
      **** MOVE S2465-GIVNAME          TO WSAA-INT1.                    <PSE30>
           MOVE S2465-LGIVNAME         TO WSAA-INT1.                    <PSE30>
      **** MOVE S2465-MIDDL-01         TO WSAA-INT2.                    <PSE30>
      **** MOVE S2465-MIDDL-02         TO WSAA-INT3.                    <PSE30>
           MOVE SPACES                 TO WSAA-INT2                     <PSE30>
                                          WSAA-INT3.                    <PSE30>
           MOVE WSAA-INTLMD            TO CLTS-INITIALS.
           IF T1680-FUNCKEYPR      NOT = '1'                            <MLS001>
            AND T1680-LANGUAGE-DBCS    = 'Y'                            <MLS001>
                MOVE SPACES            TO CLTS-INITIALS                 <MLS001>
           END-IF.                                                      <MLS001>
           MOVE S2465-MARRYD           TO CLTS-MARRYD.
           MOVE S2465-BIRTHP           TO CLTS-BIRTHP.
           MOVE S2465-CLTSEX           TO CLTS-CLTSEX.
           MOVE S2465-CLTADDR-01       TO CLTS-CLTADDR01.
           MOVE S2465-CLTADDR-02       TO CLTS-CLTADDR02.
           MOVE S2465-CLTADDR-03       TO CLTS-CLTADDR03.
           MOVE S2465-CLTADDR-04       TO CLTS-CLTADDR04.
           MOVE S2465-CLTADDR-05       TO CLTS-CLTADDR05.
      **** MOVE S2465-CLTPCODE         TO CLTS-CLTPCODE.                <GAPPH1>
           MOVE S2465-ZPTCITY          TO WSAA-CITY.                    <GAPPH1>
           MOVE S2465-ZPTDIST          TO WSAA-DIST.                    <GAPPH1>
           MOVE S2465-ZPTWARD          TO WSAA-WARD.                    <GAPPH1>
           MOVE WSAA-LOCCODE           TO CLTS-CLTPCODE.                <GAPPH1>
           MOVE S2465-CTRYCODE         TO CLTS-CTRYCODE.
           MOVE S2465-NATLTY           TO CLTS-NATLTY.
           MOVE S2465-MAILING          TO CLTS-MAILING.
           MOVE S2465-DIRMAIL          TO CLTS-DIRMAIL.
           MOVE S2465-ADDRTYPE         TO CLTS-ADDRTYPE.
           MOVE S2465-CLTPHONE-01      TO CLTS-CLTPHONE01.
           MOVE S2465-CLTPHONE-02      TO CLTS-CLTPHONE02.
           MOVE S2465-SERVBRH          TO CLTS-SERVBRH.
           MOVE S2465-STATCODE         TO CLTS-STATCODE.
           MOVE S2465-SECUITYNO        TO CLTS-SECUITYNO.
           MOVE S2465-OCCPCODE         TO CLTS-OCCPCODE.
           MOVE S2465-VIP              TO CLTS-VIP.
           MOVE S2465-CLTDOBX          TO CLTS-CLTDOB.
           MOVE S2465-CLTDODX          TO CLTS-CLTDOD.
                                                                        <V64F04>
      **** IF  S2465-CLTDODX NOT = VRCM-MAX-DATE                        <V64F04>
      ****     MOVE 'DN'      TO   CLTS-CLTSTAT.                        <V64F04>
                                                                        <V64F04>
           IF S2465-CLTDODX         NOT = VRCM-MAX-DATE                 <V64F04>
           AND S2465-CLTSTAT            = SPACES                        <V64F04>
              MOVE 'DN'                TO CLTS-CLTSTAT                  <V64F04>
           ELSE                                                         <V64F04>
           IF WSSP-SBMACTION        = 'J'                               <V64F04>
              MOVE S2465-CLTSTAT    TO CLTS-CLTSTAT                     <V64F04>
           ELSE                                                         <V64F04>
           IF WSSP-SBMACTION        = 'A'                               <V64F04>
            OR WSSP-SBMACTION       = '8'                               <V64F04>
              MOVE 'AC'             TO CLTS-CLTSTAT                     <V64F04>
           ELSE                                                         <V64F04>
           IF S2465-CLTSTAT         = SPACES                            <V64F04>
              MOVE 'AC'             TO CLTS-CLTSTAT                     <V64F04>
           ELSE                                                         <V64F04>
              MOVE S2465-CLTSTAT    TO CLTS-CLTSTAT                     <V64F04>
           END-IF.                                                      <V64F04>
                                                                        <V64F04>
           MOVE S2465-SOE              TO CLTS-SOE.
           MOVE S2465-DOCNO            TO CLTS-DOCNO.
           MOVE WSAA-CLTIND            TO CLTS-CLTIND.
           MOVE WSSP-CLNTKEY           TO CLTS-DATA-KEY.
           MOVE S2465-START-DATE       TO CLTS-START-DATE.              <025>
      **** MOVE S2465-ETHORIG          TO CLTS-ETHORIG.         <GRP2.0><025>
           MOVE S2465-NMFMT            TO CLTS-ETHORIG.                 <GRP2.0>
           MOVE S2465-LANGUAGE         TO CLTS-LANGUAGE.                <025>
           MOVE 'E'                    TO CLTS-LANGUAGE.                <NB006>
           MOVE S2465-TAXFLAG          TO CLTS-TAXFLAG.                 <V63P31>
           MOVE REWRT                  TO CLTS-FUNCTION.                <V76F10>
      **** MOVE REWRT                  TO CLTS-FUNCTION.                <V73F02>
                                                                        <V73F02>
      ***  IF  WSAA-FIRST-LOOP          = 'Y'                   <V76F10><V73F02>
      ***      MOVE '1'                TO CLTS-VALIDFLAG        <V76F10><V73F02>
      ***      MOVE WRITR              TO CLTS-FUNCTION         <V76F10><V73F02>
      ***  ELSE                                                 <V76F10><V73F02>
      ***      MOVE UPDAT              TO CLTS-FUNCTION         <V76F10><V73F02>
      ***  END-IF                                               <V76F10><V73F02>
           MOVE CLTSREC                TO CLTS-FORMAT.
      *
           CALL 'CLTSIO'                 USING CLTS-PARAMS
           IF CLTS-STATUZ              NOT = O-K
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
                                                                        <V72F01>
      * Update CLEXPF                                                   <V72F01>
           PERFORM 3800-UPDATE-CLEX.                                    <V72F01>
      * Update ZCLEPF                                                   <CLM14>
           PERFORM A3800-UPDATE-ZCLE.                                   <CLM14>
      *                                                                 <GAPPH2>
      * Update Staff Client information (SCLTPF)                        <GAPPH2>
      *                                                                 <GAPPH2>
           PERFORM 3900-UPDATE-SCLT.                                    <GAPPH2>
                                                                        <GAPPH2>
      *
      *    Update all or any aliases that exist for the client
      *
           MOVE BEGN                   TO CLRR-FUNCTION.
           MOVE CLRRREC                TO CLRR-FORMAT.
      *
           MOVE SPACES                 TO CLRR-DATA-KEY.
           MOVE PRFX-CLNT              TO CLRR-CLNTPFX.
           MOVE WSSP-FSUCO             TO CLRR-CLNTCOY.
           MOVE S2465-CLNTNUM          TO CLRR-CLNTNUM.
           MOVE 'AL'                   TO CLRR-CLRRROLE.
      *
           MOVE 'N'                    TO WSAA-END-OF-CLRR.
           PERFORM 3300-UPDATE-ALIAS   UNTIL END-OF-CLRR.
      *
      *
      *    Update all or any alternate addresses existing for the client
      *
           MOVE BEGN                   TO CLRR-FUNCTION.
           MOVE CLRRREC                TO CLRR-FORMAT.
      *
           MOVE SPACES                 TO CLRR-DATA-KEY.
           MOVE PRFX-CLNT              TO CLRR-CLNTPFX.
           MOVE WSSP-FSUCO             TO CLRR-CLNTCOY.
           MOVE S2465-CLNTNUM          TO CLRR-CLNTNUM.
           MOVE 'AA'                   TO CLRR-CLRRROLE.
      *
           MOVE 'N'                    TO WSAA-END-OF-CLRR.
           PERFORM 3500-UPDATE-ADDR    UNTIL END-OF-CLRR.
      *
           IF S2465-ZDOCTIND           NOT = WSAA-DOCTIND-STORE         <FUPLET>
                MOVE SPACES            TO CLRN-CLTRELN-REC              <FUPLET>
                IF S2465-ZDOCTIND      = 'Y'                            <FUPLET>
                   MOVE 'ADD'          TO CLRN-FUNCTION                 <FUPLET>
                ELSE                                                    <FUPLET>
                   MOVE 'REM'          TO CLRN-FUNCTION                 <FUPLET>
                END-IF                                                  <FUPLET>
                MOVE PRFX-CLNT         TO CLRN-CLNTPFX                  <FUPLET>
                MOVE WSSP-FSUCO        TO CLRN-CLNTCOY                  <FUPLET>
                MOVE S2465-CLNTNUM     TO CLRN-CLNTNUM                  <FUPLET>
                MOVE CLRF-DOCTOR       TO CLRN-CLRRROLE                 <FUPLET>
                                          CLRN-FOREPFX                  <FUPLET>
                MOVE WSSP-COMPANY      TO CLRN-FORECOY                  <FUPLET>
                MOVE S2465-CLNTNUM     TO CLRN-FORENUM                  <FUPLET>
                                                                        <FUPLET>
                CALL 'CLTRELN'      USING CLRN-CLTRELN-REC              <FUPLET>
                                                                        <FUPLET>
                IF CLRN-STATUZ      NOT = O-K                           <FUPLET>
                   MOVE CLRN-CLTRELN-REC TO SYSR-PARAMS                 <FUPLET>
                   MOVE CLRN-STATUZ      TO SYSR-STATUZ                 <FUPLET>
                   PERFORM 600-FATAL-ERROR                              <FUPLET>
                END-IF                                                  <FUPLET>
           END-IF.                                                      <FUPLET>
                                                                        <FA1971>
           PERFORM 7000-WRITE-LETTER.                                   <GAPPH2>
                                                                        <FA3292>
       003-COMMIT.                                                      <FA3292>
                                                                        <FA3292>
      *----UPDATE ENRL FILE IF THE SURNAME OR GIVNAME IS CHANGED.       <FA1971>
                                                                        <FA1971>
      **** IF WSAA-GIVNAME          NOT = S2465-LGIVNAME        <V75F01><FA1971>
      **** OR WSAA-SURNAME          NOT = S2465-LSURNAME        <V75F01><FA1971>
      ****    PERFORM A5100-STRING-CLNTNAME                     <V75F01><FA1971>
      ****    INITIALIZE                  ENRL-DATA-KEY         <V75F01><FA1971>
      ****    MOVE PRFX-CLNT           TO ENRL-PREFIX           <V75F01><FA1971>
      ****    MOVE WSSP-FSUCO          TO ENRL-COMPANY          <V75F01><FA1971>
      ****    MOVE S2465-CLNTNUM       TO ENRL-UENTITY          <V75F01><FA1971>
      ****    MOVE BEGN                TO ENRL-FUNCTION         <V75F01><FA1971>
      ****                                                      <V75F01><FA1971>
      ****    PERFORM D3000-CALL-ENRLIO                         <V75F01><FA1971>
      ****                                                      <V75F01><FA1971>
      ****    PERFORM A5200-UPDATE-ENRL                         <V75F01><FA1971>
      ****            UNTIL ENRL-STATUZ  NOT = O-K        OR    <V75F01><FA1971>
      ****                  ENRL-PREFIX  NOT = PRFX-CLNT  OR    <V75F01><FA1971>
      ****                  ENRL-COMPANY NOT = WSSP-FSUCO OR    <V75F01><FA1971>
      ****                  ENRL-UENTITY NOT = S2465-CLNTNUM    <V75F01><FA1971>
      **** END-IF.                                              <V75F01><FA1971>
                                                                        <FA1971>
           IF  WSAA-GIVNAME            NOT = S2465-LGIVNAME             <V75F01>
           OR  WSAA-SURNAME            NOT = S2465-LSURNAME             <V75F01>
           OR  WSAA-ID-CHANGED         = 'Y'                            <V75F01>
               PERFORM A5100-STRING-CLNTNAME                            <V75F01>
               PERFORM A5300-UPDATE-ENRLCLT                             <V75F01>
           END-IF.                                                      <V75F01>
      *                                                                 <V75F01>
           IF WSAA-GIVNAME          NOT = S2465-LGIVNAME                <V65F10>
           OR WSAA-SURNAME          NOT = S2465-LSURNAME                <V65F10>
           OR WSAA-ID-CHANGED           = 'Y'                           <V65F10>
              MOVE CLTS-CLNTPFX        TO CNENQ-CLNTPFX                 <V65F10>
              MOVE CLTS-CLNTCOY        TO CNENQ-CLNTCOY                 <V65F10>
              MOVE CLTS-CLNTNUM        TO CNENQ-CLNTNUM                 <V65F10>
              MOVE S2465-LGIVNAME      TO CNENQ-GIVNAME                 <V65F10>
              MOVE S2465-LSURNAME      TO CNENQ-SURNAME                 <V65F10>
              MOVE S2465-SECUITYNO     TO CNENQ-SECUITYNO               <V65F10>
              MOVE 'CLTMOD'            TO CNENQ-FUNCTION                <V65F10>
              CALL 'CNENQUP'        USING CNENQ-REC                     <V65F10>
              IF CNENQ-STATUZ       NOT = O-K                           <V65F10>
                 MOVE CNENQ-STATUZ     TO SYSR-STATUZ                   <V65F10>
                 MOVE CNENQ-REC        TO SYSR-PARAMS                   <V65F10>
                 PERFORM 600-FATAL-ERROR                                <V65F10>
              END-IF                                                    <V65F10>
              MOVE CLTS-CLNTPFX        TO AGCNQ-CLNTPFX                 <V65F10>
              MOVE CLTS-CLNTCOY        TO AGCNQ-CLNTCOY                 <V65F10>
              MOVE CLTS-CLNTNUM        TO AGCNQ-CLNTNUM                 <V65F10>
              MOVE 'UPDNAME'           TO AGCNQ-FUNCTION                <V65F10>
              CALL 'AGCNQUP'        USING AGCNQ-REC                     <V65F10>
              IF AGCNQ-STATUZ       NOT = O-K                           <V65F10>
                 MOVE AGCNQ-STATUZ     TO SYSR-STATUZ                   <V65F10>
                 MOVE AGCNQ-REC        TO SYSR-PARAMS                   <V65F10>
                 PERFORM 600-FATAL-ERROR                                <V65F10>
              END-IF                                                    <V65F10>
           END-IF.                                                      <V65F10>
                                                                        <V65F10>
       004-COMMIT.
      *                                                                 <V75F01>
           IF  WSSP-FLAG               = 'A'                            <V75F01>
           OR  WSSP-FLAG               = 'D'                            <V75F01>
               PERFORM A5400-CALL-BLDENRL                               <V75F01>
           END-IF.                                                      <V75F01>
      *
      *    Commit - must not be done in the program.
      *
       R201-EXIT.
            EXIT.
      *                                                                 <FA5288>
       3100-DELETE-CLIENT-REC SECTION.                                  <FA5288>
      *******************************                                   <FA5288>
       3100-DELETE-CLNT.                                                <FA5288>
           MOVE SPACES                 TO CLTS-PARAMS.                  <FA5288>
           MOVE WSSP-CLNTKEY           TO CLTS-DATA-KEY.                <FA5288>
           MOVE CLTSREC                TO CLTS-FORMAT.                  <FA5288>
           MOVE READH                  TO CLTS-FUNCTION.                <FA5288>
                                                                        <FA5288>
           CALL 'CLTSIO'            USING CLTS-PARAMS                   <FA5288>
           IF CLTS-STATUZ           NOT = O-K AND MRNF                  <FA5288>
              MOVE CLTS-STATUZ         TO SYSR-STATUZ                   <FA5288>
              MOVE CLTS-PARAMS         TO SYSR-PARAMS                   <FA5288>
              PERFORM 600-FATAL-ERROR                                   <FA5288>
           END-IF.                                                      <FA5288>
                                                                        <FA5288>
           IF CLTS-STATUZ               = O-K                           <FA5288>
              MOVE DELET               TO CLTS-FUNCTION                 <FA5288>
              CALL 'CLTSIO'         USING CLTS-PARAMS                   <FA5288>
                                                                        <FA5288>
              IF CLTS-STATUZ        NOT = O-K                           <FA5288>
                 MOVE CLTS-STATUZ      TO SYSR-STATUZ                   <FA5288>
                 MOVE CLTS-PARAMS      TO SYSR-PARAMS                   <FA5288>
                 PERFORM 600-FATAL-ERROR                                <FA5288>
              END-IF                                                    <FA5288>
           END-IF.                                                      <FA5288>
                                                                        <FA5288>
       3100-DELETE-CLTS.                                                <FA5288>
           MOVE SPACES                 TO CLEX-PARAMS.                  <FA5288>
           MOVE WSSP-CLNTKEY           TO CLEX-DATA-KEY.                <FA5288>
           MOVE CLEXREC                TO CLEX-FORMAT.                  <FA5288>
           MOVE READH                  TO CLEX-FUNCTION.                <FA5288>
           CALL 'CLEXIO'            USING CLEX-PARAMS.                  <FA5288>
           IF CLEX-STATUZ           NOT = O-K AND MRNF                  <FA5288>
              MOVE CLEX-STATUZ         TO SYSR-STATUZ                   <FA5288>
              MOVE CLEX-PARAMS         TO SYSR-PARAMS                   <FA5288>
              PERFORM 600-FATAL-ERROR                                   <FA5288>
           END-IF.                                                      <FA5288>
                                                                        <FA5288>
           IF CLEX-STATUZ               = O-K                           <FA5288>
              MOVE DELET               TO CLEX-FUNCTION                 <FA5288>
              CALL 'CLEXIO'         USING CLEX-PARAMS                   <FA5288>
              IF CLEX-STATUZ        NOT = O-K                           <FA5288>
                 MOVE CLEX-STATUZ      TO SYSR-STATUZ                   <FA5288>
                 MOVE CLEX-PARAMS      TO SYSR-PARAMS                   <FA5288>
                 PERFORM 600-FATAL-ERROR                                <FA5288>
              END-IF                                                    <FA5288>
           END-IF.                                                      <FA5288>
       3100-EXIT.                                                       <FA5288>
           EXIT.                                                        <FA5288>
      *
       3300-UPDATE-ALIAS SECTION.
      ***************************
      *
      *    Update all or any aliases that exist for the client
      *
       3310-READ-CLRR.
      *
           CALL 'CLRRIO'               USING CLRR-PARAMS.
      *
           IF CLRR-STATUZ              NOT = O-K AND
              CLRR-STATUZ              NOT = ENDP
               MOVE CLRR-PARAMS        TO SYSR-PARAMS
               MOVE CLRR-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *
           IF CLRR-STATUZ              = ENDP OR
              CLRR-CLNTPFX             NOT = PRFX-CLNT OR
              CLRR-CLNTCOY             NOT = WSSP-FSUCO OR
              CLRR-CLNTNUM             NOT = S2465-CLNTNUM OR
              CLRR-CLRRROLE            NOT = 'AL'
                 MOVE 'Y'              TO WSAA-END-OF-CLRR
                 GO 3390-EXIT.
      *
           IF CLRR-USED-TO-BE          NOT = SPACES
              GO 3380-NEXTR.
      *
           PERFORM 3700-READ-CLTS.
      *
           PERFORM R201-UPDATE-CLTS.
      *
       3380-NEXTR.
      *
           MOVE NEXTR                  TO CLRR-FUNCTION .
      *
       3390-EXIT.
           EXIT.
      *
      *                                                                 <027>
       3400-BEGIN-CLRR SECTION.                                         <027>
      *************************                                         <027>
      *                                                                 <027>
      *    Check whether any more aliases or alternate address          <027>
      *    records exist for the client.                                <027>
      *                                                                 <027>
       3410-READ-CLRR.                                                  <027>
      *                                                                 <027>
           MOVE BEGN                   TO CLRR-FUNCTION.                <027>
           MOVE CLRRREC                TO CLRR-FORMAT.                  <027>
      *                                                                 <027>
           MOVE SPACES                 TO CLRR-DATA-KEY.                <027>
           MOVE PRFX-CLNT              TO CLRR-CLNTPFX.                 <027>
           MOVE WSSP-FSUCO             TO CLRR-CLNTCOY.                 <027>
           MOVE WSAA-CLNTNUM           TO CLRR-CLNTNUM.                 <027>
           MOVE WSAA-CLRROLE           TO CLRR-CLRRROLE.                <027>
      *                                                                 <027>
           CALL 'CLRRIO'               USING CLRR-PARAMS.               <027>
      *                                                                 <027>
           IF CLRR-STATUZ              NOT = O-K AND                    <027>
              CLRR-STATUZ              NOT = ENDP                       <027>
               MOVE CLRR-PARAMS        TO SYSR-PARAMS                   <027>
               MOVE CLRR-STATUZ        TO SYSR-STATUZ                   <027>
               PERFORM 600-FATAL-ERROR.                                 <027>
      *                                                                 <027>
           IF CLRR-STATUZ              = ENDP OR                        <027>
              CLRR-CLNTPFX             NOT = PRFX-CLNT OR               <027>
              CLRR-CLNTCOY             NOT = WSSP-FSUCO OR              <027>
              CLRR-CLNTNUM             NOT = WSAA-CLNTNUM OR            <027>
              CLRR-CLRRROLE            NOT = WSAA-CLRROLE               <027>
              PERFORM 3600-UPDATE-ROLE                                  <027>
           END-IF.                                                      <027>
      *                                                                 <027>
       3490-EXIT.                                                       <027>
           EXIT.                                                        <027>
      *                                                                 <027>
       3500-UPDATE-ADDR SECTION.
      **************************
      *
      *    Update all or any alternate addresses existing for the client
      *
       3510-READ-CLRR.
      *
           CALL 'CLRRIO'                 USING CLRR-PARAMS.
      *
           IF CLRR-STATUZ              NOT = O-K AND
              CLRR-STATUZ              NOT = ENDP
               MOVE CLRR-PARAMS        TO SYSR-PARAMS
               MOVE CLRR-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *
           IF CLRR-STATUZ              = ENDP OR
              CLRR-CLNTPFX             NOT = PRFX-CLNT OR
              CLRR-CLNTCOY             NOT = WSSP-FSUCO OR
              CLRR-CLNTNUM             NOT = S2465-CLNTNUM OR
              CLRR-CLRRROLE            NOT = 'AA'
                 MOVE 'Y'              TO WSAA-END-OF-CLRR
                 GO 3590-EXIT.
      *
           IF CLRR-USED-TO-BE          NOT = SPACE
              GO 3580-NEXT-CLRR.
      *
           PERFORM 3700-READ-CLTS.
      *
           PERFORM R201-UPDATE-CLTS.
      *
       3580-NEXT-CLRR.
      *
           MOVE NEXTR                  TO CLRR-FUNCTION.
      *
       3590-EXIT.
           EXIT.
      *                                                                 <027>
       3600-UPDATE-ROLE SECTION.                                        <027>
      **************************                                        <027>
      *                                                                 <027>
      *    Update role flag for the Master client.                      <027>
      *                                                                 <027>
       3610-READ-CLTS.                                                  <027>
      *                                                                 <027>
      *                                                                 <027>
           MOVE SPACES                 TO CLTS-DATA-AREA.               <027>
           MOVE PRFX-CLNT              TO CLTS-CLNTPFX.                 <027>
           MOVE WSSP-FSUCO             TO CLTS-CLNTCOY.                 <027>
           MOVE WSAA-CLNTNUM           TO CLTS-CLNTNUM.                 <027>
      *                                                                 <027>
           MOVE READH                  TO CLTS-FUNCTION.                <027>
           CALL 'CLTSIO'               USING CLTS-PARAMS.               <027>
           IF CLTS-STATUZ              NOT = O-K                        <027>
               MOVE CLTS-PARAMS        TO SYSR-PARAMS                   <027>
               PERFORM 600-FATAL-ERROR.                                 <027>
      *                                                                 <027>
      ***  MOVE '2'                    TO CLTS-VALIDFLAG.       <V76F10><V73F02>
                                                                        <V73F02>
      ***  MOVE REWRT                  TO CLTS-FUNCTION.        <V76F10><V73F02>
      ***  CALL 'CLTSIO'            USING CLTS-PARAMS.          <V76F10><V73F02>
                                                                        <V73F02>
      ***  IF  CLTS-STATUZ          NOT = O-K                   <V76F10><V73F02>
      ***      MOVE CLTS-PARAMS        TO SYSR-PARAMS           <V76F10><V73F02>
      ***      PERFORM 600-FATAL-ERROR                          <V76F10><V73F02>
      ***  END-IF.                                              <V76F10><V73F02>
                                                                        <V73F02>
           IF WSAA-CLRROLE             = 'AA'                           <027>
              MOVE SPACES              TO CLTS-ROLEFLAG01               <027>
           END-IF.                                                      <027>
           IF WSAA-CLRROLE             = 'AL'                           <027>
            AND CLTS-ROLEFLAG03    NOT = 'U'                            <027>
              MOVE SPACES              TO CLTS-ROLEFLAG03               <027>
           END-IF.                                                      <027>
      **** MOVE REWRT                  TO CLTS-FUNCTION.        <V73F02><027>
                                                                        <V73F02>
      ***  MOVE '1'                    TO CLTS-VALIDFLAG.       <V76F10><V73F02>
                                                                        <V73F02>
      ***  MOVE WRITR                  TO CLTS-FUNCTION.        <V76F10><V73F02>
           MOVE REWRT                  TO CLTS-FUNCTION.                <V76F10>
           MOVE CLTSREC                TO CLTS-FORMAT.                  <027>
      *                                                                 <027>
           CALL 'CLTSIO'               USING CLTS-PARAMS.               <027>
           IF CLTS-STATUZ              NOT = O-K                        <027>
               MOVE CLTS-PARAMS        TO SYSR-PARAMS                   <027>
               PERFORM 600-FATAL-ERROR.                                 <027>
      *                                                                 <027>
       3690-EXIT.                                                       <027>
           EXIT.                                                        <027>

       3700-READ-CLTS SECTION.
      ***********************
      *
       3710-START.
      *
           MOVE SPACES                 TO CLTS-DATA-AREA.
           MOVE CLRR-FOREPFX           TO CLTS-CLNTPFX.
           MOVE CLRR-FORECOY           TO CLTS-CLNTCOY.
           MOVE CLRR-FORENUM           TO CLTS-CLNTNUM.
      *
           MOVE READH                  TO CLTS-FUNCTION.
           CALL 'CLTSIO'               USING CLTS-PARAMS.
           IF CLTS-STATUZ              NOT = O-K
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
       3790-EXIT.
           EXIT.
                                                                        <V72F01>
       3800-UPDATE-CLEX SECTION.                                        <V72F01>
       3800-PARA.                                                       <V72F01>
      *                                                                 <V72F01>
           MOVE SPACES                 TO CLEX-PARAMS.                  <V72F01>
           MOVE WSSP-CLNTKEY           TO CLEX-DATA-KEY.                <V72F01>
           MOVE CLEXREC                TO CLEX-FORMAT.                  <V72F01>
           MOVE READR                  TO CLEX-FUNCTION.                <V72F01>
           CALL 'CLEXIO'                 USING CLEX-PARAMS.             <V72F01>
           IF CLEX-STATUZ              NOT = O-K AND MRNF               <V72F01>
               MOVE CLEX-PARAMS        TO SYSR-PARAMS                   <V72F01>
               PERFORM 600-FATAL-ERROR                                  <V72F01>
           END-IF.                                                      <V72F01>
           IF CLEX-STATUZ               = O-K                           <V72F01>
              IF S2465-RINTERNET        = SPACE AND                     <V72F01>
                 S2465-RMBLPHONE        = SPACE                         <V72F01>
             AND CLEX-IDDATE            = VRCM-MAX-DATE                 <GAPPH1>
             AND CLEX-IDPLACE           = SPACES                        <GAPPH1>
                 IF CLEX-RDIDTELNO      = SPACE AND                     <V72F01>
                    CLEX-RPAGER         = SPACE AND                     <V72F01>
                    CLEX-FAXNO          = SPACE AND                     <V72F01>
                    CLEX-RTAXIDNUM      = SPACE AND                     <V72F01>
                    CLEX-RSTAFLAG       = SPACE AND                     <V72F01>
                    CLEX-ZSPECIND       = SPACE AND                     <V72F01>
                    CLEX-OLDIDNO        = SPACE                         <V72F01>
      ****          MOVE DELET         TO CLEX-FUNCTION         <V73F02><V72F01>
                    MOVE '2'           TO CLEX-VALIDFLAG                <V73F02>
                    MOVE WRITD         TO CLEX-FUNCTION                 <V73F02>
                    CALL 'CLEXIO'   USING CLEX-PARAMS                   <V72F01>
                    IF CLEX-STATUZ   NOT = O-K                          <V72F01>
                       MOVE CLEX-PARAMS TO SYSR-PARAMS                  <V72F01>
                       PERFORM 600-FATAL-ERROR                          <V72F01>
                    END-IF                                              <V72F01>
                    GO TO 3800-EXIT                                     <V72F01>
                 END-IF                                                 <V72F01>
              END-IF                                                    <V72F01>
      ****    MOVE UPDAT               TO CLEX-FUNCTION         <V73F02><V72F01>
              MOVE UPDAT               TO CLEX-FUNCTION                 <V76F10>
      ***     IF  S2465-RINTERNET   NOT = CLEX-RINTERNET        <V76F10><V73F02>
      ***     OR  S2465-RMBLPHONE   NOT = CLEX-RMBLPHONE        <V76F10><V73F02>
      ***         MOVE '2'             TO CLEX-VALIDFLAG        <V76F10><V73F02>
      ***         MOVE WRITD           TO CLEX-FUNCTION         <V76F10><V73F02>
      ***         CALL 'CLEXIO'     USING CLEX-PARAMS           <V76F10><V73F02>
      ***         IF  CLEX-STATUZ   NOT = O-K                   <V76F10><V73F02>
      ***             MOVE CLEX-PARAMS TO SYSR-PARAMS           <V76F10><V73F02>
      ***             PERFORM 600-FATAL-ERROR                   <V76F10><V73F02>
      ***         END-IF                                        <V76F10><V73F02>
      ***     ELSE                                              <V76F10><V73F02>
      ***         GO TO 3800-EXIT                               <V76F10><V73F02>
      ***     END-IF                                            <V76F10><V73F02>
           ELSE                                                         <V72F01>
      ****    IF S2465-RINTERNET        = SPACE AND             <GAPPH1><V72F01>
      ****       S2465-RMBLPHONE        = SPACE                 <GAPPH1><V72F01>
      ****       GO TO 3800-EXIT                                <GAPPH1><V72F01>
      ****    END-IF                                            <GAPPH1><V72F01>
              INITIALIZE CLEX-DATA-AREA                                 <V72F01>
              MOVE WSSP-CLNTKEY        TO CLEX-DATA-KEY                 <V72F01>
              MOVE UPDAT               TO CLEX-FUNCTION                 <V76F10>
      ****    MOVE UPDAT               TO CLEX-FUNCTION         <V73F02><V72F01>
           END-IF.                                                      <V72F01>
           MOVE S2465-RINTERNET        TO CLEX-RINTERNET                <V72F01>
           MOVE S2465-RMBLPHONE        TO CLEX-RMBLPHONE.               <V72F01>
      ***  MOVE '1'                    TO CLEX-VALIDFLAG.       <V76F10><V73F02>
           MOVE '1'                    TO CLEX-VALIDFLAG.               <FA5297>
           MOVE S2465-IDDATE           TO CLEX-IDDATE.                  <GAPPH1>
           MOVE S2465-IDPLACE          TO CLEX-IDPLACE.                 <GAPPH1>
                                                                        <V72F01>
      ***  MOVE WRITR                  TO CLEX-FUNCTION.        <V76F10><V73F02>
           CALL 'CLEXIO'                 USING CLEX-PARAMS.             <V72F01>
                                                                        <V72F01>
           IF CLEX-STATUZ              NOT = O-K                        <V72F01>
               MOVE CLEX-PARAMS        TO SYSR-PARAMS                   <V72F01>
               PERFORM 600-FATAL-ERROR                                  <V72F01>
           END-IF.                                                      <V72F01>
      *                                                                 <V72F01>
       3800-EXIT.                                                       <V72F01>
           EXIT.                                                        <V72F01>
      *                                                                 <GAPPH2>
       A3800-UPDATE-ZCLE SECTION.                                       <CLM14>
      ****************************                                      <CLM14>
       A3810-START.                                                     <CLM14>
      *                                                                 <CLM14>
           MOVE SPACES                 TO ZCLE-PARAMS.                  <CLM14>
           MOVE WSSP-CLNTKEY           TO ZCLE-DATA-KEY.                <CLM14>
           MOVE ZCLEREC                TO ZCLE-FORMAT.                  <CLM14>
           MOVE READR                  TO ZCLE-FUNCTION.                <CLM14>
           CALL 'ZCLEIO'            USING ZCLE-PARAMS.                  <CLM14>
           IF ZCLE-STATUZ           NOT = O-K AND MRNF                  <CLM14>
               MOVE ZCLE-PARAMS        TO SYSR-PARAMS                   <CLM14>
               PERFORM 600-FATAL-ERROR                                  <CLM14>
           END-IF.                                                      <CLM14>
           IF ZCLE-STATUZ               = O-K                           <CLM14>
              IF S2465-IDPLACEXT        = SPACES                        <CLM14>
                 MOVE '2'              TO ZCLE-VALIDFLAG                <CLM14>
                 MOVE WRITD            TO ZCLE-FUNCTION                 <CLM14>
                 CALL 'ZCLEIO'      USING ZCLE-PARAMS                   <CLM14>
                 IF ZCLE-STATUZ     NOT = O-K                           <CLM14>
                    MOVE ZCLE-PARAMS   TO SYSR-PARAMS                   <CLM14>
                    PERFORM 600-FATAL-ERROR                             <CLM14>
                 END-IF                                                 <CLM14>
                 GO TO A3890-EXIT                                       <CLM14>
              ELSE                                                      <CLM14>
                 MOVE UPDAT            TO ZCLE-FUNCTION                 <CLM14>
              END-IF                                                    <CLM14>
           ELSE                                                         <CLM14>
              INITIALIZE ZCLE-DATA-AREA                                 <CLM14>
              MOVE WSSP-CLNTKEY        TO ZCLE-DATA-KEY                 <CLM14>
              MOVE UPDAT               TO ZCLE-FUNCTION                 <CLM14>
           END-IF.                                                      <CLM14>
           MOVE '1'                    TO ZCLE-VALIDFLAG.               <CLM14>
           MOVE S2465-IDDATE           TO ZCLE-IDDATE.                  <CLM14>
           MOVE S2465-IDPLACE          TO ZCLE-IDPLACE.                 <CLM14>
           MOVE S2465-IDPLACEXT        TO ZCLE-IDPLACEXT.               <CLM14>
                                                                        <CLM14>
           CALL 'ZCLEIO'                 USING ZCLE-PARAMS.             <CLM14>
                                                                        <CLM14>
           IF ZCLE-STATUZ              NOT = O-K                        <CLM14>
               MOVE ZCLE-PARAMS        TO SYSR-PARAMS                   <CLM14>
               PERFORM 600-FATAL-ERROR                                  <CLM14>
           END-IF.                                                      <CLM14>
                                                                        <CLM14>
       A3890-EXIT.                                                      <CLM14>
             EXIT.                                                      <CLM14>
      /                                                                 <GAPPH2>
       3900-UPDATE-SCLT SECTION.                                        <GAPPH2>
      **************************                                        <GAPPH2>
      *                                                                 <GAPPH2>
       3910-START.                                                      <GAPPH2>
      *                                                                 <GAPPH2>
           IF S2465-CANFLAG             = WSAA-STAFFLAG-SAV             <GAPPH2>
              GO TO 3990-EXIT                                           <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           MOVE SPACES                 TO SCLTENQ-PARAMS.               <GAPPH2>
           MOVE WSSP-CLNTKEY           TO SCLTENQ-DATA-KEY.             <GAPPH2>
           MOVE SCLTENQREC             TO SCLTENQ-FORMAT.               <GAPPH2>
           MOVE READR                  TO SCLTENQ-FUNCTION.             <GAPPH2>
                                                                        <GAPPH2>
           CALL 'SCLTENQIO'         USING SCLTENQ-PARAMS.               <GAPPH2>
                                                                        <GAPPH2>
           IF SCLTENQ-STATUZ        NOT = O-K AND MRNF                  <GAPPH2>
               MOVE SCLTENQ-PARAMS     TO SYSR-PARAMS                   <GAPPH2>
               PERFORM 600-FATAL-ERROR                                  <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           IF SCLTENQ-STATUZ            = O-K                           <GAPPH2>
              MOVE '2'                 TO SCLTENQ-VALIDFLAG             <GAPPH2>
              MOVE WSAA-TODAY          TO SCLTENQ-CURRTO                <GAPPH2>
              MOVE WRITD               TO SCLTENQ-FUNCTION              <GAPPH2>
              CALL 'SCLTENQIO'      USING SCLTENQ-PARAMS                <GAPPH2>
              IF SCLTENQ-STATUZ     NOT = O-K                           <GAPPH2>
                 MOVE SCLTENQ-PARAMS TO SYSR-PARAMS                     <GAPPH2>
                 PERFORM 600-FATAL-ERROR                                <GAPPH2>
              END-IF                                                    <GAPPH2>
           ELSE                                                         <GAPPH2>
              INITIALIZE SCLTENQ-DATA-AREA                              <GAPPH2>
              MOVE WSSP-CLNTKEY        TO SCLTENQ-DATA-KEY              <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           MOVE '1'                    TO SCLTENQ-VALIDFLAG.            <GAPPH2>
           MOVE WSAA-TODAY             TO SCLTENQ-CURRFROM.             <GAPPH2>
           MOVE VRCM-MAX-DATE          TO SCLTENQ-CURRTO.               <GAPPH2>
      *                                                                 <GAPPH2>
      * "Y" for Staff and NO print billing notice;                      <GAPPH2>
      * "N" for Staff  and print billing notice.                        <GAPPH2>
      * Defaut is Blank for normally cases.                             <GAPPH2>
      *                                                                 <GAPPH2>
           IF   S2465-CANFLAG       NOT = SPACES                        <GAPPH2>
                MOVE 'Y'               TO SCLTENQ-RSTAFLAG              <GAPPH2>
           ELSE                                                         <GAPPH2>
                MOVE 'N'               TO SCLTENQ-RSTAFLAG              <GAPPH2>
           END-IF.                                                      <GAPPH2>
           IF   S2465-CANFLAG           = 'N' OR SPACES                 <GAPPH2>
                MOVE 'Y'               TO SCLTENQ-PRTFLG                <GAPPH2>
           ELSE                                                         <GAPPH2>
                MOVE 'N'               TO SCLTENQ-PRTFLG                <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           MOVE UPDAT                  TO SCLTENQ-FUNCTION.             <GAPPH2>
           CALL 'SCLTENQIO'         USING SCLTENQ-PARAMS.               <GAPPH2>
                                                                        <GAPPH2>
           IF SCLTENQ-STATUZ        NOT = O-K                           <GAPPH2>
               MOVE SCLTENQ-PARAMS     TO SYSR-PARAMS                   <GAPPH2>
               PERFORM 600-FATAL-ERROR                                  <GAPPH2>
           END-IF.                                                      <GAPPH2>
      *                                                                 <GAPPH2>
       3990-EXIT.                                                       <GAPPH2>
           EXIT.                                                        <GAPPH2>
      *                                                                 <GAPPH2>
      /                                                                 <GAPPH2>
      *
       R201-UPDATE-CLTS SECTION.
      **************************
      *
       3910-START.
      *
      ***  MOVE '2'                    TO CLTS-VALIDFLAG.       <V76F10><V73F02>
                                                                        <V73F02>
      ***  MOVE REWRT                  TO CLTS-FUNCTION.        <V76F10><V73F02>
      ***  MOVE CLTSREC                TO CLTS-FORMAT.          <V76F10><V73F02>
      ***  CALL 'CLTSIO'            USING CLTS-PARAMS.          <V76F10><V73F02>
                                                                        <V73F02>
      ***  IF  CLTS-STATUZ          NOT = O-K                   <V76F10><V73F02>
      ***      MOVE CLTS-PARAMS        TO SYSR-PARAMS           <V76F10><V73F02>
      ***      PERFORM 600-FATAL-ERROR                          <V76F10><V73F02>
      ***  END-IF.                                              <V76F10><V73F02>
                                                                        <V73F02>
           MOVE VRCM-TRANID            TO CLTS-TRANID.
           MOVE S2465-MARRYD           TO CLTS-MARRYD.
           MOVE S2465-BIRTHP           TO CLTS-BIRTHP.
           MOVE S2465-CLTSEX           TO CLTS-CLTSEX.
           MOVE S2465-NATLTY           TO CLTS-NATLTY.
           MOVE S2465-MAILING          TO CLTS-MAILING.
           MOVE S2465-DIRMAIL          TO CLTS-DIRMAIL.
      **** MOVE S2465-ADDRTYPE         TO CLTS-ADDRTYPE.                <A05918>
      **** MOVE S2465-CLTPHONE-01      TO CLTS-CLTPHONE01.              <PS027>
      **** MOVE S2465-CLTPHONE-02      TO CLTS-CLTPHONE02.              <PS027>
           MOVE S2465-SERVBRH          TO CLTS-SERVBRH.
           MOVE S2465-STATCODE         TO CLTS-STATCODE.
           MOVE S2465-SECUITYNO        TO CLTS-SECUITYNO.
           MOVE S2465-OCCPCODE         TO CLTS-OCCPCODE.
           MOVE S2465-VIP              TO CLTS-VIP.
           MOVE S2465-CLTDOBX          TO CLTS-CLTDOB.
           MOVE S2465-CLTDODX          TO CLTS-CLTDOD.
           IF  S2465-CLTDODX NOT = VRCM-MAX-DATE
               MOVE 'DN'      TO   CLTS-CLTSTAT.
           MOVE S2465-SOE              TO CLTS-SOE.
           MOVE S2465-DOCNO            TO CLTS-DOCNO.
           MOVE S2465-START-DATE       TO CLTS-START-DATE.              <025>
      **** MOVE S2465-ETHORIG          TO CLTS-ETHORIG.         <GRP2.0><025>
           MOVE S2465-NMFMT            TO CLTS-ETHORIG.                 <GRP2.0>
           MOVE S2465-LANGUAGE         TO CLTS-LANGUAGE.                <025>
           MOVE S2465-TAXFLAG          TO CLTS-TAXFLAG.                 <V63P31>
      ***  MOVE '1'                    TO CLTS-VALIDFLAG.       <V76F10><V73F02>
      *
           IF CLRR-CLRRROLE            = 'AA'
              GO 3950-ADDR.
      *
           MOVE S2465-CLTADDR-01       TO CLTS-CLTADDR01.
           MOVE S2465-CLTADDR-02       TO CLTS-CLTADDR02.
           MOVE S2465-CLTADDR-03       TO CLTS-CLTADDR03.
           MOVE S2465-CLTADDR-04       TO CLTS-CLTADDR04.
           MOVE S2465-CLTADDR-05       TO CLTS-CLTADDR05.
      **** MOVE S2465-CLTPCODE         TO CLTS-CLTPCODE.                <GAPPH1>
           MOVE S2465-ZPTCITY          TO WSAA-CITY.                    <GAPPH1>
           MOVE S2465-ZPTDIST          TO WSAA-DIST.                    <GAPPH1>
           MOVE S2465-ZPTWARD          TO WSAA-WARD.                    <GAPPH1>
           MOVE WSAA-LOCCODE           TO CLTS-CLTPCODE.                <GAPPH1>
           MOVE S2465-CTRYCODE         TO CLTS-CTRYCODE.
           MOVE S2465-ADDRTYPE         TO CLTS-ADDRTYPE.                <A05918>
           MOVE S2465-CLTPHONE-01      TO CLTS-CLTPHONE01.              <PS027>
           MOVE S2465-CLTPHONE-02      TO CLTS-CLTPHONE02.              <PS027>
      *
           GO 3970-REWRITE-CLTS.
      *
       3950-ADDR.
      *
      **** MOVE S2465-SURNAME          TO CLTS-SURNAME.                 <PSE30>
      **** MOVE S2465-GIVNAME          TO CLTS-GIVNAME.                 <PSE30>
           IF T3711-FLAG = 'Y'                                          <PSE30>
              MOVE S2465-LSURNAME      TO CLTS-GIVNAME                  <PSE30>
              MOVE S2465-LGIVNAME      TO CLTS-SURNAME                  <PSE30>
           ELSE                                                         <PSE30>
              MOVE S2465-LSURNAME      TO CLTS-SURNAME                  <PSE30>
              MOVE S2465-LGIVNAME      TO CLTS-GIVNAME                  <PSE30>
           END-IF.                                                      <PSE30>
           MOVE S2465-LSURNAME         TO CLTS-LSURNAME.                <PSE30>
           MOVE S2465-LGIVNAME         TO CLTS-LGIVNAME.                <PSE30>
      **** MOVE S2465-SALUT            TO CLTS-SALUTL.          <WLI.P2><CAS2.0>
           MOVE S2465-SALUTL           TO CLTS-SALUTL.                  <CAS2.0>
      **** MOVE S2465-MIDDL-01         TO CLTS-MIDDL01.                 <PSE30>
      **** MOVE S2465-MIDDL-02         TO CLTS-MIDDL02.                 <PSE30>
           MOVE SPACES                 TO CLTS-MIDDL01                  <PSE30>
                                          CLTS-MIDDL02.                 <PSE30>
      **** MOVE S2465-GIVNAME          TO WSAA-INT1.                    <PSE30>
           MOVE S2465-LGIVNAME          TO WSAA-INT1.                   <PSE30>
      **** MOVE S2465-MIDDL-01         TO WSAA-INT2.                    <PSE30>
      **** MOVE S2465-MIDDL-02         TO WSAA-INT3.                    <PSE30>
           MOVE SPACES                 TO WSAA-INT2                     <PSE30>
                                          WSAA-INT3.                    <PSE30>
           MOVE WSAA-INTLMD            TO CLTS-INITIALS.
           IF T1680-FUNCKEYPR      NOT = '1'                            <MLS001>
            AND T1680-LANGUAGE-DBCS    = 'Y'                            <MLS001>
                MOVE SPACES            TO CLTS-INITIALS                 <MLS001>
           END-IF.                                                      <MLS001>
      *
       3970-REWRITE-CLTS.
      *
      **** MOVE REWRT                  TO CLTS-FUNCTION.                <V73F02>
      ***  MOVE WRITR                  TO CLTS-FUNCTION.        <V76F10><V73F02>
           MOVE REWRT                  TO CLTS-FUNCTION.                <V76F10>
           MOVE CLTSREC                TO CLTS-FORMAT.
      *
           CALL 'CLTSIO'               USING CLTS-PARAMS.
           IF CLTS-STATUZ              NOT = O-K
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
       3990-EXIT.
           EXIT.
      *
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
       4010-NEXT-PROGRAM.
      *

           MOVE WSSP-COMPANY           TO GENS-COMPANY.
           MOVE WSAA-PROG              TO GENS-PROG-IN.
           MOVE WSSP-BATCHKEY          TO WSKY-BATC-FILE-KEY.
           MOVE WSKY-BATC-BATCTRCDE    TO GENS-TRANSACT.
           MOVE WSAA-PROG              TO WSSP-NEXTPROG.
      *
      *  If first time into this section (stack action blank)
      *  save next eight programs in stack
      *
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = SPACE
              MOVE WSSP-PROGRAM-PTR    TO WSAA-X
              MOVE 1                   TO WSAA-Y
              PERFORM 4100-SAVE-PROGRAM 8 TIMES.
      *                                                                 <CN005>
      *  Check if extra fields indicator selected previously            <CN005>
      *                                                                 <CN005>
           IF S2465-REXTRFLD = '?'                                      <CN005>
              PERFORM A4000-CHECK-CLEX                                  <CN005>
           END-IF.                                                      <CN005>
                                                                        <FA4547>
      *    Check salary history indicator                               <FA4547>
      *                                                                 <FA4547>
           IF S2465-UK-PENSION-IND      = '?'                           <FA4547>
              PERFORM A7000-CHECK-SOIN                                  <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
      *    Check sub-standatd retention indicator                       <FA4547>
      *                                                                 <FA4547>
           IF S2465-RACRIND             = '?'                           <FA4547>
              PERFORM A8000-CHECK-RACR                                  <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
      *    Check bankrupt indicator                                     <FA4547>
      *                                                                 <FA4547>
           IF S2465-BRUPIND             = '?'                           <FA4547>
              PERFORM A9000-CHECK-BNKRPT                                <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
      *  AML Profiling                                                  <FA4547>
      *                                                                 <FA4547>
           IF S2465-CLPRFIND            = '?'                           <FA4547>
              PERFORM A10000-CHECK-CLPR                                 <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
      *
      *  Check if either of the functon keys were pressed
      *
           IF SCRN-STATUZ = 'DIRY'
              MOVE O-K            TO SCRN-STATUZ
              MOVE 'D'            TO GENS-FUNCTION
              PERFORM 4300-CALL-GENSSW
              GO TO 4090-EXIT.
      *
           IF SCRN-STATUZ = 'SERV'
              MOVE O-K            TO SCRN-STATUZ
              MOVE 'L'            TO GENS-FUNCTION
              PERFORM 4300-CALL-GENSSW
              GO TO 4090-EXIT.
      *
           IF SCRN-STATUZ               = KILL
              MOVE SPACES           TO  WSSP-SEC-PROG(WSSP-PROGRAM-PTR)
                                        WSSP-SEC-ACTN(WSSP-PROGRAM-PTR)
                                        WSSP-VALUE
      ****     MOVE 'HIDEW'          TO SCRN-FUNCTION           <V72F01><029>
      *        CALL 'S2465IO' USING SCRN-SCREEN-PARAMS                  <029>
      *                             S2465-DATA-AREA                     <029>
              GO TO  4090-EXIT
           END-IF.
      *
      *  Check if salary history selected
      *
           IF S2465-UK-PENSION-IND = 'X'
   ****       MOVE SPACES         TO S2465-UK-PENSION-IND               <FA4547>
              MOVE '?'            TO S2465-UK-PENSION-IND               <FA4547>
              MOVE 'U'            TO GENS-FUNCTION
              PERFORM 4300-CALL-GENSSW
              GO TO 4090-EXIT.
      *
           IF S2465-RACRIND        = 'X'                                <FA4547>
              MOVE '?'            TO S2465-RACRIND                      <FA4547>
              MOVE 'Z'            TO GENS-FUNCTION                      <FA4547>
              IF WSSP-FLAG         = 'A'                                <FA4547>
                  MOVE 'C'        TO WSSP-FLAG                          <FA4547>
              END-IF                                                    <FA4547>
              PERFORM 4300-CALL-GENSSW                                  <FA4547>
              GO TO 4090-EXIT                                           <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
           IF S2465-BRUPIND       = 'X'                                 <V71L02>
   ****       MOVE SPACES         TO S2465-BRUPIND              <FA4547><V71L02>
              MOVE '?'            TO S2465-BRUPIND                      <FA4547>
              MOVE 'B'            TO GENS-FUNCTION                      <V71L02>
              PERFORM 4300-CALL-GENSSW                                  <V71L02>
              GO TO 4090-EXIT.                                          <V71L02>
                                                                        <V71L02>
      **** IF S2465-CLPRFIND      = 'X'                         <FA4547><V74F03>
      ****    MOVE SPACES         TO S2465-CLPRFIND             <FA4547><V74F03>
      ****    MOVE 'C'            TO GENS-FUNCTION              <FA4547><V74F03>
      ****    PERFORM 4300-CALL-GENSSW                          <FA4547><V74F03>
      ****    GO TO 4090-EXIT.                                  <FA4547><V74F03>
      ****                                                      <FA4547><V74F03>
      **** IF S2465-RACRIND        = 'X'                        <FA4547><R96REA>
      ****    MOVE SPACES         TO S2465-RACRIND              <FA4547><R96REA>
      ****    MOVE 'Z'            TO GENS-FUNCTION              <FA4547><R96REA>
      ****    IF WSSP-FLAG         = 'A'                        <FA4547><R96REA>
      ****        MOVE 'C'        TO WSSP-FLAG                  <FA4547><R96REA>
      ****    END-IF                                            <FA4547><R96REA>
      ****    PERFORM 4300-CALL-GENSSW                          <FA4547><R96REA>
      ****    GO TO 4090-EXIT                                   <FA4547><R96REA>
      **** END-IF.                                              <FA4547><R96REA>
      *                                                                 <CN005>
      *  Check if Additional Field selected                             <CN005>
      *                                                                 <CN005>
           IF S2465-REXTRFLD = 'X'                                      <CN005>
              MOVE '?'            TO S2465-REXTRFLD                     <CN005>
              MOVE 'A'            TO GENS-FUNCTION                      <CN005>
              PERFORM 4300-CALL-GENSSW                                  <CN005>
              GO TO 4090-EXIT.                                          <CN005>
      *                                                                 <FA4547>
      *  AML Profiling should be last checkbox                          <FA4547>
      *                                                                 <FA4547>
           IF S2465-CLPRFIND      = 'X'                                 <FA4547>
              MOVE '?'            TO S2465-CLPRFIND                     <FA4547>
              MOVE 'C'            TO GENS-FUNCTION                      <FA4547>
              PERFORM 4300-CALL-GENSSW                                  <FA4547>
              GO TO 4090-EXIT.                                          <FA4547>
                                                                        <FA4547>
      *                                                         <R96REA>
      *   No more selected (or none)
      *      - restore stack form wsaa to wssp
      *
           MOVE WSSP-PROGRAM-PTR       TO WSAA-X
           MOVE 1                      TO WSAA-Y
           PERFORM 4200-RESTORE-PROGRAM 8 TIMES.
      *
      *  If current stack action is * then re-display screen
      *     (in this case, some other option(s) were requested)
      *  Otherwise continue as normal.
      *

           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
               MOVE SPACE            TO WSSP-SEC-ACTN(WSSP-PROGRAM-PTR)
               MOVE SCRN-SCRNAME     TO WSSP-NEXTPROG
           ELSE
      ****     MOVE 'HIDEW'          TO SCRN-FUNCTION           <V72F01><029>
      *        CALL 'S2465IO' USING SCRN-SCREEN-PARAMS                  <029>
      *                             S2465-DATA-AREA                     <029>
                 ADD 1               TO WSSP-PROGRAM-PTR.               <S9503>
      *
       4090-EXIT.
            EXIT.
      *
      *
       4100-SAVE-PROGRAM        SECTION.
      ***********************************
       4110-SAVE.
           MOVE WSSP-SEC-PROG (WSAA-X) TO WSAA-SEC-PROG(WSAA-Y).
           ADD 1                       TO WSAA-X.
           ADD 1                       TO WSAA-Y.
      *
       4190-EXIT.
            EXIT.
      *
       4200-RESTORE-PROGRAM      SECTION.
      ***********************************
       4210-RESTORE.
           MOVE WSAA-SEC-PROG (WSAA-Y) TO WSSP-SEC-PROG(WSAA-X).
           ADD 1                       TO WSAA-X.
           ADD 1                       TO WSAA-Y.
       4290-EXIT.
            EXIT.
      *
       4300-CALL-GENSSW          SECTION.
      ***********************************
       4310-CALL-SUBROUTINE.
           CALL 'GENSSW' USING GENS-GENSSW-REC.
           IF GENS-STATUZ NOT = O-K
                                AND NOT = MRNF
               MOVE GENS-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR.
      *****
      * If an entry on T1675 was not found by genswch redisplay the scree015>
      * with an error and the options and extras indicator
      * with its initial load value
      *****
           IF GENS-STATUZ               = MRNF
              MOVE ' '                 TO WSSP-SEC-ACTN
                                              (WSSP-PROGRAM-PTR)
              MOVE H093                TO SCRN-ERROR-CODE
              MOVE SCRN-SCRNAME        TO WSSP-NEXTPROG
                 GO TO 4390-EXIT.
      *
      *    load from gensw to wssp
      *
           ADD 1, WSSP-PROGRAM-PTR GIVING WSAA-X
           MOVE 1                      TO WSAA-Y
           PERFORM 4400-LOAD-PROGRAM 8 TIMES.
      *
           MOVE '*'                TO WSSP-SEC-ACTN (WSSP-PROGRAM-PTR).
           ADD 1                       TO WSSP-PROGRAM-PTR.
      *
       4390-EXIT.
            EXIT.
      *
       4400-LOAD-PROGRAM         SECTION.
      ***********************************
       4210-RESTORE.
           MOVE GENS-PROG-OUT (WSAA-Y) TO WSSP-SEC-PROG(WSAA-X).
           ADD 1                       TO WSAA-X.
           ADD 1                       TO WSAA-Y.
       4290-EXIT.
            EXIT.
      /
      *********************************                                 <030>
       6000-READ-ADDR-VAL-TABLE SECTION.                                <030>
      *********************************                                 <030>
       6010-INIT.                                                       <030>
      *                                                                 <030>
      *  Read Address validation rule Table                             <030>
      *                                                                 <030>
           MOVE SPACES                 TO ITEM-DATA-KEY.                <030>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <030>
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.                 <030>
           MOVE T2241                  TO ITEM-ITEMTABL.                <030>
********** MOVE CLTS-CTRYCODE          TO WSAA-CNT.             <A05912><030>
           MOVE WSAA-CLTS-CTRYCODE     TO WSAA-CNT.                     <A05912>
           MOVE WSSP-LANGUAGE          TO WSAA-LNG.                     <030>
           MOVE WSAA-CNT-LNG           TO ITEM-ITEMITEM.                <030>
           MOVE READR                  TO ITEM-FUNCTION.                <030>
      *                                                                 <030>
           CALL 'ITEMIO' USING ITEM-PARAMS.                             <030>
           IF ITEM-STATUZ          NOT = O-K                            <030>
                               AND NOT = MRNF                           <030>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <030>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <030>
               PERFORM 600-FATAL-ERROR                                  <030>
           END-IF.                                                      <030>
      *                                                                 <030>
           IF ITEM-STATUZ              = MRNF                           <030>
      *                                                                 <030>
      *  Read Default validation if specific not found                  <030>
      *                                                                 <030>
               MOVE SPACES             TO ITEM-DATA-KEY                 <030>
               MOVE 'IT'               TO ITEM-ITEMPFX                  <030>
               MOVE WSSP-FSUCO         TO ITEM-ITEMCOY                  <030>
               MOVE T2241              TO ITEM-ITEMTABL                 <030>
               MOVE '***'              TO WSAA-CNT                      <030>
               MOVE WSSP-LANGUAGE      TO WSAA-LNG                      <030>
               MOVE WSAA-CNT-LNG       TO ITEM-ITEMITEM                 <030>
               MOVE READR              TO ITEM-FUNCTION                 <030>
      *                                                                 <030>
               CALL 'ITEMIO' USING ITEM-PARAMS                          <030>
               IF ITEM-STATUZ      NOT = O-K                            <030>
                               AND NOT = MRNF                           <030>
                   MOVE ITEM-PARAMS    TO SYSR-PARAMS                   <030>
                   MOVE ITEM-STATUZ    TO SYSR-STATUZ                   <030>
                   PERFORM 600-FATAL-ERROR                              <030>
               END-IF                                                   <030>
      *                                                                 <030>
           END-IF.                                                      <030>
      *                                                                 <030>
           IF ITEM-STATUZ          NOT = O-K                            <030>
               MOVE D009               TO S2465-CLTADDR-ERR (1)         <030>
           ELSE                                                         <030>
               MOVE ITEM-GENAREA       TO T2241-T2241-REC.              <030>
      *                                                                 <030>
       6090-EXIT.                                                       <030>
           EXIT.                                                        <030>
      /                                                                 <030>
      *********************************                                 <030>
       6100-VALIDATE-ADDRESS    SECTION.                                <030>
      *********************************                                 <030>
       6110-INIT.                                                       <030>
      *                                                                 <030>
      *  Read Address validation rule Table                             <030>
      *                                                                 <030>
      *-----------------------------------------------------------------<V5L001>
      * If ALTERNATIVE LANGUAGE is used and is capable of DBCS          <V5L001>
      *    skip address validation.                                     <V5L001>
      *-----------------------------------------------------------------<V5L001>
           IF T1680-FUNCKEYPR      NOT = '1'                            <V5L001>
            AND T1680-LANGUAGE-DBCS    = 'Y'                            <V5L001>
              GO TO 6190-EXIT.                                          <V5L001>
                                                                        <V5L001>
           PERFORM 6000-READ-ADDR-VAL-TABLE.                            <030>
      *                                                                 <030>
      *                                                                 <030>
      *                                                                 <030>
      *  This validity check makes sure that at least one of the        <030>
      *  rules held on T2241 applies otherwise the address lines        <030>
      *  are flagged with an error.                                     <030>
      *                                                                 <030>
      *  Check for validity using 1st Address rules.                    <030>
      *                                                                 <030>
           MOVE 'Y'   TO WSAA-CHECK.                                    <030>
      *                                                                 <030>
           PERFORM VARYING WSAA-SUB FROM 1 BY 1                         <030>
                           UNTIL WSAA-SUB > 5                           <030>
      ****     EVALUATE T2241-ADDRINDA (WSAA-SUB) NOT = SPACES          <030>
      ****         ALSO S2465-CLTADDR  (WSAA-SUB) NOT = SPACES          <030>
      * The table entry (T2241) specifies valid rules (M,N,O).  <A07142><030>
      * CLTADDR line should meet one of the specified rule.     <A07142><030>
      * If this criteria is met then the address has been entered       <A07142>
      * correctly.  If not check continues with next address rules.     <A07142>
      ****         WHEN TRUE  ALSO FALSE                        <A07142><030>
      ****             MOVE 'N'   TO WSAA-CHECK                 <A07142><030>
      ****         WHEN FALSE ALSO TRUE                         <A07142><030>
      ****             MOVE 'N'   TO WSAA-CHECK                 <A07142><030>
                                                                        <A07142>
                EVALUATE T2241-ADDRINDA (WSAA-SUB)                      <A07142>
                   WHEN 'M'                                             <A07142>
                     IF S2465-CLTADDR (WSAA-SUB) = SPACES               <A07142>
                        MOVE 'N'           TO WSAA-CHECK                <A07142>
                     END-IF                                             <A07142>
                                                                        <A07142>
                   WHEN 'N'                                             <A07142>
                     IF S2465-CLTADDR (WSAA-SUB) NOT = SPACES           <A07142>
                        MOVE 'N'           TO WSAA-CHECK                <A07142>
                     END-IF                                             <A07142>
      *                                                                 <A07142>
               END-EVALUATE                                             <030>
           END-PERFORM.                                                 <030>
      *                                                                 <030>
           IF WSAA-OK                                                   <030>
               GO TO 6190-EXIT.                                         <030>
      *                                                                 <030>
      *  Check for validity using 2nd Address rules.                    <030>
      *                                                                 <030>
           MOVE 'Y'   TO WSAA-CHECK.                                    <030>
      *                                                                 <030>
           PERFORM VARYING WSAA-SUB FROM 1 BY 1                         <030>
                           UNTIL WSAA-SUB > 5                           <030>
      ****     EVALUATE T2241-ADDRINDB (WSAA-SUB) NOT = SPACES  <A07142><030>
      ****         ALSO S2465-CLTADDR  (WSAA-SUB) NOT = SPACES  <A07142><030>
      ****         WHEN TRUE  ALSO FALSE                        <A07142><030>
      ****             MOVE 'N'   TO WSAA-CHECK                 <A07142><030>
      ****         WHEN FALSE ALSO TRUE                         <A07142><030>
      ****             MOVE 'N'   TO WSAA-CHECK                 <A07142><030>
                                                                        <A07142>
                EVALUATE T2241-ADDRINDB (WSAA-SUB)                      <A07142>
                   WHEN 'M'                                             <A07142>
                     IF S2465-CLTADDR (WSAA-SUB) = SPACES               <A07142>
                        MOVE 'N'           TO WSAA-CHECK                <A07142>
                     END-IF                                             <A07142>
                                                                        <A07142>
                   WHEN 'N'                                             <A07142>
                     IF S2465-CLTADDR (WSAA-SUB) NOT = SPACES           <A07142>
                        MOVE 'N'           TO WSAA-CHECK                <A07142>
                     END-IF                                             <A07142>
               END-EVALUATE                                             <030>
           END-PERFORM.                                                 <030>
      *                                                                 <030>
           IF WSAA-OK                                                   <030>
               GO TO 6190-EXIT.                                         <030>
      *                                                                 <030>
      *                                                                 <030>
      *  Check for validity using 3rd Address rules.                    <030>
      *                                                                 <030>
           MOVE 'Y'   TO WSAA-CHECK.                                    <030>
      *                                                                 <030>
           PERFORM VARYING WSAA-SUB FROM 1 BY 1                         <030>
                           UNTIL WSAA-SUB > 5                           <030>
      ****     EVALUATE T2241-ADDRINDC (WSAA-SUB) NOT = SPACES  <A07142><030>
      ****         ALSO S2465-CLTADDR  (WSAA-SUB) NOT = SPACES  <A07142><030>
      ****         WHEN TRUE  ALSO FALSE                        <A07142><030>
      ****             MOVE 'N'   TO WSAA-CHECK                 <A07142><030>
      ****         WHEN FALSE ALSO TRUE                         <A07142><030>
      ****             MOVE 'N'   TO WSAA-CHECK                 <A07142><030>
                                                                        <A07142>
                EVALUATE T2241-ADDRINDC (WSAA-SUB)                      <A07142>
                   WHEN 'M'                                             <A07142>
                     IF S2465-CLTADDR (WSAA-SUB) = SPACES               <A07142>
                        MOVE 'N'           TO WSAA-CHECK                <A07142>
                     END-IF                                             <A07142>
                                                                        <A07142>
                   WHEN 'N'                                             <A07142>
                     IF S2465-CLTADDR (WSAA-SUB) NOT = SPACES           <A07142>
                        MOVE 'N'           TO WSAA-CHECK                <A07142>
                     END-IF                                             <A07142>
               END-EVALUATE                                             <030>
           END-PERFORM.                                                 <030>
      *                                                                 <030>
           IF WSAA-OK                                                   <030>
               GO TO 6190-EXIT.                                         <030>
      *                                                                 <030>
      *                                                                 <030>
      *  Check for validity using 4th Address rules.                    <030>
      *                                                                 <030>
           MOVE 'Y'   TO WSAA-CHECK.                                    <030>
      *                                                                 <030>
           PERFORM VARYING WSAA-SUB FROM 1 BY 1                         <030>
                           UNTIL WSAA-SUB > 5                           <030>
      ****     EVALUATE T2241-ADDRINDD (WSAA-SUB) NOT = SPACES  <A07142><030>
      ****         ALSO S2465-CLTADDR  (WSAA-SUB) NOT = SPACES  <A07142><030>
      ****         WHEN TRUE  ALSO FALSE                        <A07142><030>
      ****             MOVE 'N'   TO WSAA-CHECK                 <A07142><030>
      ****         WHEN FALSE ALSO TRUE                         <A07142><030>
      ****             MOVE 'N'   TO WSAA-CHECK                 <A07142><030>
                                                                        <A07142>
                EVALUATE T2241-ADDRINDD (WSAA-SUB)                      <A07142>
                   WHEN 'M'                                             <A07142>
                     IF S2465-CLTADDR (WSAA-SUB) = SPACES               <A07142>
                        MOVE 'N'           TO WSAA-CHECK                <A07142>
                     END-IF                                             <A07142>
                                                                        <A07142>
                   WHEN 'N'                                             <A07142>
                     IF S2465-CLTADDR (WSAA-SUB) NOT = SPACES           <A07142>
                        MOVE 'N'           TO WSAA-CHECK                <A07142>
                     END-IF                                             <A07142>
               END-EVALUATE                                             <030>
           END-PERFORM.                                                 <030>
      *                                                                 <030>
           IF WSAA-OK                                                   <030>
               GO TO 6190-EXIT.                                         <030>
      *                                                                 <030>
      *                                                                 <030>
      *  Check for validity using 5th Address rules.                    <030>
      *                                                                 <030>
           MOVE 'Y'   TO WSAA-CHECK.                                    <030>
      *                                                                 <030>
           PERFORM VARYING WSAA-SUB FROM 1 BY 1                         <030>
                           UNTIL WSAA-SUB > 5                           <030>
      ****     EVALUATE T2241-ADDRINDE (WSAA-SUB) NOT = SPACES  <A07142><030>
      ****         ALSO S2465-CLTADDR  (WSAA-SUB) NOT = SPACES  <A07142><030>
      ****         WHEN TRUE  ALSO FALSE                        <A07142><030>
      ****             MOVE 'N'   TO WSAA-CHECK                 <A07142><030>
      ****         WHEN FALSE ALSO TRUE                         <A07142><030>
      ****             MOVE 'N'   TO WSAA-CHECK                 <A07142><030>
                                                                        <A07142>
                EVALUATE T2241-ADDRINDE (WSAA-SUB)                      <A07142>
                   WHEN 'M'                                             <A07142>
                     IF S2465-CLTADDR (WSAA-SUB) = SPACES               <A07142>
                        MOVE 'N'           TO WSAA-CHECK                <A07142>
                     END-IF                                             <A07142>
                                                                        <A07142>
                   WHEN 'N'                                             <A07142>
                     IF S2465-CLTADDR (WSAA-SUB) NOT = SPACES           <A07142>
                        MOVE 'N'           TO WSAA-CHECK                <A07142>
                     END-IF                                             <A07142>
               END-EVALUATE                                             <030>
           END-PERFORM.                                                 <030>
      *                                                                 <030>
           IF WSAA-OK                                                   <030>
               GO TO 6190-EXIT.                                         <030>
      *                                                                 <030>
           MOVE G987               TO S2465-CLTADDR-ERR (1)             <030>
                                      S2465-CLTADDR-ERR (2)             <030>
                                      S2465-CLTADDR-ERR (3)             <030>
                                      S2465-CLTADDR-ERR (4)             <030>
                                      S2465-CLTADDR-ERR (5).            <030>
      *                                                                 <030>
       6190-EXIT.                                                       <030>
           EXIT.                                                        <030>
      /                                                                 <030>
      *****************************************************************
       A0000-MISCELLANEOUS SECTION.
      *****************************
       A1000-READ-ITEM.
      *
           MOVE READR                  TO ITEM-FUNCTION.
           CALL 'ITEMIO'                 USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = MRNF AND
              ITEM-STATUZ              NOT = O-K
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
      *A1000-EXIT.
      *     EXIT.
      *
       A2000-GETDESC.
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE 'IT'                   TO DESC-DESCPFX.
           MOVE WSSP-FSUCO             TO DESC-DESCCOY.
           MOVE ITEM-ITEMTABL          TO DESC-DESCTABL.
           MOVE ITEM-ITEMITEM          TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE 'READR'                TO DESC-FUNCTION.
           CALL 'DESCIO' USING DESC-PARAMS.
           IF DESC-STATUZ              NOT = O-K
                                         AND MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
           IF DESC-STATUZ = MRNF
               MOVE '???????'          TO DESC-LONGDESC,
                                          DESC-SHORTDESC.
      *
      *  The exit paragraph name will be standardised to 2090 -EXIT.    <S9503>
      *A2900-EXIT.                                                      <S9503>
      *A2090-EXIT.                                                      <S9503>
      *     EXIT.
      /
       A3000-VALNAME SECTION.
      ************************
       A3000-VALIDATE.
      ****==============================================================<032>
      ****  Character validation now uses T3716.                        <032>
      ****==============================================================<032>
              MOVE O-K                 TO VALN-STATUZ.

      *-----------------------------------------------------------------<V5L001>
      * If ALTERNATIVE LANGUAGE is used and is capable of DBCS          <V5L001>
      *    skip validation of name & address                            <V5L001>
      *-----------------------------------------------------------------<V5L001>
           IF T1680-FUNCKEYPR       NOT = '1'                           <V5L001>
            AND T1680-LANGUAGE-DBCS     = 'Y'                           <V5L001>
              GO TO AR201-EXIT                                          <V5L001>
           END-IF.                                                      <V5L001>
      *
      *    Check that first character is not a space
      *
           IF WSAA-CHAR(1)             = SPACES
              MOVE G986                TO VALN-STATUZ
              GO TO AR201-EXIT.
      *
      *    Check that there are not two spaces together
      *
      **** MOVE 31                     TO WSAA-INDEX.                   <PSE30>
           MOVE 61                     TO WSAA-INDEX.                   <PSE30>
       410-LOOP1.
           SUBTRACT 1                  FROM WSAA-INDEX.
           IF WSAA-CHAR(WSAA-INDEX)    NOT = SPACE
              GO TO 420-TEST-SPACES.
           IF WSAA-INDEX               > 2
              GO TO 410-LOOP1.
           GO TO 490-EXIT.
      *
       420-TEST-SPACES.
           MOVE WSAA-INDEX             TO WSAA-INDEXA.
           SUBTRACT 1                  FROM WSAA-INDEXA.
      *
       430-LOOP2.
           SUBTRACT 1                  FROM WSAA-INDEX
                                            WSAA-INDEXA.
           IF WSAA-CHAR(WSAA-INDEX)    = SPACE  AND
              WSAA-CHAR(WSAA-INDEXA)   = SPACE
              MOVE G986                TO VALN-STATUZ
              GO TO AR201-EXIT.
           IF WSAA-INDEX               > 2
              GO TO 430-LOOP2.
       490-EXIT.
      *
      *    Check that there are not more than three identical           <033>
      *    characters together.                                         <033>
      ****                                                              <032>
      **** Check that all characters are valid                          <032>
      **** Note - the first character must be an upper case letter      <032>
<032> **** MOVE 0                      TO WSAA-INDEX.
<032> *610-LOOP1.
<032> **** ADD 1                       TO WSAA-INDEX.
<032> **** MOVE 0                      TO WSAA-INDEXA.
                                                                        <025>
           MOVE 1                      TO WSAA-INDEX.                   <025>
           MOVE 0                      TO WSAA-INDEXA.                  <025>
           MOVE 2                      TO WSAA-INDEXB.                  <025>
           MOVE 3                      TO WSAA-INDEXC.                  <033>
                                                                        <025>
       510-LOOP.                                                        <025>
           ADD 1                       TO WSAA-INDEX                    <025>
                                          WSAA-INDEXA                   <025>
                                          WSAA-INDEXB                   <025>
                                          WSAA-INDEXC.                  <033>
                                                                        <025>
           IF WSAA-CHAR(WSAA-INDEX)    = SPACE                          <025>
              GO TO 530-NEXT.                                           <025>
           IF WSAA-CHAR(WSAA-INDEX)    NUMERIC                          <025>
              GO TO 530-NEXT.                                           <025>
           IF WSAA-CHAR(WSAA-INDEX)    = WSAA-CHAR(WSAA-INDEXA) AND     <025>
              WSAA-CHAR(WSAA-INDEX)    = WSAA-CHAR(WSAA-INDEXB) AND     <025>
              WSAA-CHAR(WSAA-INDEX)    = WSAA-CHAR(WSAA-INDEXC)         <033>
              MOVE G986                TO VALN-STATUZ                   <025>
              GO TO AR201-EXIT.                                         <025>
       530-NEXT.                                                        <025>
      **** IF WSAA-INDEX               < 29                     <PSE30> <025>
           IF WSAA-INDEX               < 59                             <PSE30>
              GO TO 510-LOOP.                                           <025>
      *                                                                 <025>
      *    Check that all characters are valid                          <025>
      *    Note - the first character must be an upper case letter      <025>
      *                                                                 <025>
      *    Not true in all languages.                                   <025>
      *                                                                 <025>
           MOVE 0                      TO WSAA-INDEX.                   <025>
      *                                                                 <025>
       610-LOOP1.                                                       <025>
           ADD 1                       TO WSAA-INDEX.                   <025>
           MOVE 0                      TO WSAA-INDEXA.                  <025>
      *                                                                 <V5L001>
      *    Ignore the character check if WSAA-VALID-CHARS = SPACES      <V5L001>
      *                                                                 <V5L001>
           IF WSAA-ARRAY = SPACES                                       <V5L001>
              MOVE 60 TO WSAA-INDEX                                     <V5L001>
              GO TO 630-NEXT                                            <V5L001>
           END-IF.                                                      <V5L001>
      *                                                                 <V5L001>
      *                                                                 <025>
      *    Address lines may have numeric chracters, commas and full    <025>
      *                                                           stops <025>
      *    and now '#' !                                                <028>
           IF VALN-FUNCTION = VALN-ADDRESS                              <025>
            AND (WSAA-CHAR(WSAA-INDEX) NUMERIC                          <025>
             OR (WSAA-CHAR(WSAA-INDEX) = ',' AND WSAA-INDEX NOT = 1)    <025>
             OR (WSAA-CHAR(WSAA-INDEX) = '.' AND WSAA-INDEX NOT = 1)    <025>
      ****   OR (WSAA-CHAR(WSAA-INDEX) = '/' AND WSAA-INDEX NOT = 1))   <028>
             OR (WSAA-CHAR(WSAA-INDEX) = '/' AND WSAA-INDEX NOT = 1)    <028>
             OR (WSAA-CHAR(WSAA-INDEX) = '#'                       ))   <028>
              GO TO 630-NEXT.                                           <025>
                                                                        <CAS1.0>
           IF VALN-FUNCTION = VALN-NAME                                 <CAS1.0>
           AND  (WSAA-CHAR(WSAA-INDEX) = '/' AND WSAA-INDEX NOT = 1)    <CAS1.0>
              GO TO 630-NEXT.                                           <CAS1.0>
      *                                                                 <025>
       620-LOOP2.                                                       <025>
           ADD 1                       TO WSAA-INDEXA.                  <025>
      *                                                                 <025>
      *    Character check here                                         <025>
      *                                                                 <025>
           IF WSAA-CHAR(WSAA-INDEX)    = WSAA-VALID-CHAR(WSAA-INDEXA)   <025>
              GO TO 630-NEXT.                                           <025>
      *****IF (WSAA-INDEX NOT = 1 AND WSAA-INDEXA < WSAA-NO-OF-CHAR)<A06288><025
      ***** OR (WSAA-INDEX = 1 AND WSAA-INDEXA < 26)               <025><032>
      ***** OR (WSAA-INDEX = 1 AND WSAA-INDEXA < 40)            <A06288><032>
           IF WSAA-INDEXA < WSAA-NO-OF-CHAR                             <A06288>
              GO TO 620-LOOP2.                                          <025>
      *                                                                 <025>
      *    Not found - invalid character                                <025>
      *                                                                 <025>
      *    MOVE G986                   TO VALN-STATUZ.             <025><032>
           MOVE D036                   TO VALN-STATUZ.                  <032>
           GO TO AR201-EXIT.                                            <025>
      *                                                                 <025>
       630-NEXT.                                                        <025>
      **** IF WSAA-INDEX               < 30                     <PSE30> <025>
           IF WSAA-INDEX               < 60                             <PSE30>
              GO TO 610-LOOP1.                                          <025>
      *
      *    Check that there is at least one string of 2 or more chars
      *
           MOVE 1                      TO WSAA-INDEX.
           MOVE 0                      TO WSAA-INDEXA.
      *
       710-LOOP.
           ADD 1                       TO WSAA-INDEX
                                          WSAA-INDEXA.
      *
      *    Check for consecutive characters
      *
           IF WSAA-CHAR(WSAA-INDEX)    NOT = SPACE AND
              WSAA-CHAR(WSAA-INDEXA)   NOT = SPACE
              GO TO 790-EXIT.
      *
       730-NEXT.
      **** IF WSAA-INDEX               < 30                             <PSE30>
           IF WSAA-INDEX               < 60                             <PSE30>
              GO TO 710-LOOP.
      *
      *    No string of two characters has been found
      *
           MOVE G986                   TO VALN-STATUZ.
           GO TO AR201-EXIT.
      *
       790-EXIT.
      *
      *    Check for incorrect character combinations
      *
           MOVE 1                      TO WSAA-INDEX.
           MOVE 0                      TO WSAA-INDEXA.
      *
       810-LOOP.
           ADD 1                       TO WSAA-INDEX
                                          WSAA-INDEXA.
      *
      *    Check that hyphen is not next to blank
      *
           IF WSAA-CHAR(WSAA-INDEX)    = SPACE AND
              WSAA-CHAR(WSAA-INDEXA)   = '-'
           AND VALN-FUNCTION           = VALN-ADDRESS                   <DA014>
              MOVE G986                TO VALN-STATUZ
              GO TO AR201-EXIT.
           IF WSAA-CHAR(WSAA-INDEXA)   = SPACE AND
              WSAA-CHAR(WSAA-INDEX)    = '-'
           AND VALN-FUNCTION           = VALN-ADDRESS                   <DA014>
              MOVE G986                TO VALN-STATUZ
              GO TO AR201-EXIT.
      *
      *    Check that apostrophe is not next to blank
      *
           IF WSAA-CHAR(WSAA-INDEX)    = SPACE AND
              WSAA-CHAR(WSAA-INDEXA)   = "'"
           AND VALN-FUNCTION           = VALN-ADDRESS                   <DA014>
              MOVE G986                TO VALN-STATUZ
              GO TO AR201-EXIT.
           IF WSAA-CHAR(WSAA-INDEXA)   = SPACE AND
              WSAA-CHAR(WSAA-INDEX)    = "'"
              MOVE G986                TO VALN-STATUZ
              GO TO AR201-EXIT.
      *
      *    Check that a comma immedatly follows a character
      *      and is always followed by a space  (address lines only)
      *
           IF WSAA-CHAR(WSAA-INDEX)    NOT = SPACE AND
              WSAA-CHAR(WSAA-INDEXA)   = ','
              MOVE G986                TO VALN-STATUZ
              GO TO AR201-EXIT.
           IF WSAA-CHAR(WSAA-INDEXA)   = SPACE AND
              WSAA-CHAR(WSAA-INDEX)    = ','
              MOVE G986                TO VALN-STATUZ
              GO TO AR201-EXIT.
      *
      **** IF WSAA-INDEX               < 30                             <PSE30>
           IF WSAA-INDEX               < 60                             <PSE30>
              GO TO 810-LOOP.
      *
       AR201-EXIT.
            EXIT.
      /
      /                                                                 <CN005>
      **************************                                        <CN005>
       A4000-CHECK-CLEX SECTION.                                        <CN005>
      **************************                                        <CN005>
       A4010-READ-CLEX.                                                 <CN005>
      *                                                                 <CN005>
           MOVE SPACES                 TO CLEX-PARAMS.                  <CN005>
           MOVE WSSP-CLNTKEY           TO CLEX-DATA-KEY.                <CN005>
           MOVE CLEXREC                TO CLEX-FORMAT.                  <CN005>
           MOVE READR                  TO CLEX-FUNCTION.                <CN005>
           CALL 'CLEXIO'                 USING CLEX-PARAMS.             <CN005>
           IF CLEX-STATUZ              NOT = O-K AND MRNF               <CN005>
               MOVE CLEX-PARAMS        TO SYSR-PARAMS                   <CN005>
               PERFORM 600-FATAL-ERROR.                                 <CN005>
                                                                        <CN005>
           IF CLEX-STATUZ = MRNF                                        <CN005>
               MOVE SPACE              TO S2465-REXTRFLD                <CN005>
           ELSE                                                         <CN005>
      ****     MOVE '+'                TO S2465-REXTRFLD        <V72F01><CN005>
               IF CLEX-RDIDTELNO    NOT = SPACE OR                      <V72F01>
                  CLEX-RPAGER       NOT = SPACE OR                      <V72F01>
                  CLEX-FAXNO        NOT = SPACE OR                      <V72F01>
                  CLEX-RTAXIDNUM    NOT = SPACE OR                      <V72F01>
                  CLEX-RSTAFLAG     NOT = SPACE OR                      <V72F01>
                  CLEX-ZSPECIND     NOT = SPACE OR                      <V72F01>
                  CLEX-OLDIDNO      NOT = SPACE                         <V72F01>
                  MOVE '+'             TO S2465-REXTRFLD                <V72F01>
               ELSE                                                     <V72F01>
                  MOVE SPACES          TO S2465-REXTRFLD                <V72F01>
               END-IF                                                   <V72F01>
           END-IF.                                                      <CN005>
                                                                        <CN005>
       A4090-EXIT.                                                      <CN005>
            EXIT.                                                       <CN005>
      /                                                                 <CN005>
       B4000-CHECK-ZCLE SECTION.                                        <CLM14>
      ***************************                                       <CLM14>
       B4010-START.                                                     <CLM14>
      *                                                                 <CLM14>
           MOVE SPACES                 TO ZCLE-PARAMS.                  <CLM14>
           MOVE WSSP-CLNTKEY           TO ZCLE-DATA-KEY.                <CLM14>
           MOVE ZCLEREC                TO ZCLE-FORMAT.                  <CLM14>
           MOVE READR                  TO ZCLE-FUNCTION.                <CLM14>
                                                                        <CLM14>
           CALL 'ZCLEIO'                 USING ZCLE-PARAMS.             <CLM14>
                                                                        <CLM14>
           IF ZCLE-STATUZ              NOT = O-K AND MRNF               <CLM14>
               MOVE ZCLE-PARAMS        TO SYSR-PARAMS                   <CLM14>
               PERFORM 600-FATAL-ERROR.                                 <CLM14>
      *                                                                 <CLM14>
       B4090-EXIT.                                                      <CLM14>
             EXIT.                                                      <CLM14>
      /                                                                 <CLM14>
       A5000-SANCTION-CLIENT SECTION.                                   <ICA011>
      *******************************                                   <ICA011>
       A5010-SACTION.                                                   <ICA011>
      *                                                                 <ICA011>
      *  Checking whether is SDA or non SDA by calling CLNENRL          <ICA011>
      *  sub-routine.It will be automatically granted or revoked        <ICA011>
      *  that client if it is an SDA user. If non SDA user, it will     <ICA011>
      *  exit without sanction.                                         <ICA011>
      *                                                                 <ICA011>
           INITIALIZE                  CLNENRL-CLNENRL-REC.             <ICA011>
           IF WSSP-FLAG                = 'D'                            <ICA011>
              MOVE 'DLSN'              TO CLNENRL-FUNCTION              <ICA011>
           ELSE                                                         <ICA011>
              MOVE 'CHSN'              TO CLNENRL-FUNCTION              <ICA011>
           END-IF.                                                      <ICA011>
           MOVE WSSP-USERID            TO CLNENRL-USERID.               <ICA011>
           MOVE WSSP-FSUCO             TO CLNENRL-FSUCO.                <ICA011>
           MOVE SPACES                 TO CLNENRL-CLRRROLE.             <ICA011>
           MOVE PRFX-CLNT              TO CLNENRL-CLNTPFX.              <ICA011>
           MOVE WSSP-FSUCO             TO CLNENRL-CLNTCOY.              <ICA011>
           MOVE S2465-CLNTNUM          TO CLNENRL-CLNTNUM.              <ICA011>
                                                                        <ICA011>
           CALL 'CLNENRL'              USING CLNENRL-CLNENRL-REC.       <ICA011>
                                                                        <ICA011>
           IF CLNENRL-STATUZ           NOT = O-K                        <ICA011>
              MOVE CLNENRL-STATUZ      TO SYSR-STATUZ                   <ICA011>
              PERFORM 600-FATAL-ERROR                                   <ICA011>
           END-IF.                                                      <ICA011>
      *                                                                 <ICA011>
       A5090-EXIT.                                                      <ICA011>
           EXIT.                                                        <ICA011>
      /                                                                 <ICA011>
       A5100-STRING-CLNTNAME SECTION.                                   <FA1971>
      ******************************                                    <FA1971>
       A5110-STRING.                                                    <FA1971>
      *                                                                 <FA1971>
           MOVE SPACES                 TO WSAA-CNV-SURNAME              <FA1971>
                                          WSAA-CNV-GIVNAME              <FA1971>
                                          WSAA-CNV-CLNTNAME.            <FA1971>
           MOVE S2465-LSURNAME         TO DBCS-INPUT-STRING.            <FA1971>
           MOVE 30                     TO DBCS-OUTPUT-LENGTH.           <FA1971>
           MOVE SPACES                 TO DBCS-STATUZ.                  <FA1971>
           MOVE SPACES                 TO DBCS-OUTPUT-STRING.           <FA1971>
           CALL 'DBCSTRNC'          USING DBCSTRNC-REC                  <FA1971>
           IF DBCS-STATUZ           NOT = O-K                           <FA1971>
              MOVE SPACES              TO DBCS-OUTPUT-STRING            <FA1971>
           END-IF.                                                      <FA1971>
           MOVE DBCS-OUTPUT-STRING     TO WSAA-CNV-SURNAME.             <FA1971>
                                                                        <FA1971>
           MOVE S2465-LGIVNAME         TO DBCS-INPUT-STRING.            <FA1971>
           MOVE 20                     TO DBCS-OUTPUT-LENGTH.           <FA1971>
           MOVE SPACES                 TO DBCS-STATUZ.                  <FA1971>
           MOVE SPACES                 TO DBCS-OUTPUT-STRING.           <FA1971>
           CALL 'DBCSTRNC'          USING DBCSTRNC-REC.                 <FA1971>
           IF DBCS-STATUZ           NOT = O-K                           <FA1971>
              MOVE SPACES              TO DBCS-OUTPUT-STRING            <FA1971>
           END-IF                                                       <FA1971>
           MOVE DBCS-OUTPUT-STRING     TO WSAA-CNV-GIVNAME.             <FA1971>
                                                                        <FA1971>
           STRING S2465-LSURNAME    DELIMITED BY '  ',                  <FA1971>
                  ' '               DELIMITED BY SIZE                   <FA1971>
                  S2465-LGIVNAME    DELIMITED BY '  '                   <FA1971>
                  INTO WSAA-CLNTNAME.                                   <FA1971>
           MOVE WSAA-CLNTNAME          TO DBCS-INPUT-STRING.            <FA1971>
           MOVE 47                     TO DBCS-OUTPUT-LENGTH.           <FA1971>
           MOVE SPACES                 TO DBCS-STATUZ.                  <FA1971>
           MOVE SPACES                 TO DBCS-OUTPUT-STRING.           <FA1971>
           CALL 'DBCSTRNC'          USING DBCSTRNC-REC.                 <FA1971>
           IF DBCS-STATUZ           NOT = O-K                           <FA1971>
              MOVE SPACES              TO DBCS-OUTPUT-STRING            <FA1971>
           END-IF.                                                      <FA1971>
           MOVE DBCS-OUTPUT-STRING     TO WSAA-CNV-CLNTNAME.            <FA1971>
                                                                        <FA1971>
       A5190-EXIT.                                                      <FA1971>
           EXIT.                                                        <FA1971>
      /                                                                 <FA1971>
       A5200-UPDATE-ENRL SECTION.                                       <FA1971>
      **************************                                        <FA1971>
       A5210-ENRL.                                                      <FA1971>
      *                                                                 <FA1971>
      *  WRITD means directly rewritten an existing record              <FA1971>
      *  based on RRN                                                   <FA1971>
      *                                                                 <FA1971>
           MOVE WSAA-CNV-SURNAME       TO ENRL-SURNAME.                 <FA1971>
           MOVE WSAA-CNV-GIVNAME       TO ENRL-GIVNAME.                 <FA1971>
           MOVE WSAA-CNV-CLNTNAME      TO ENRL-CLNTNAME.                <FA1971>
                                                                        <FA1971>
           MOVE WRITD                  TO ENRL-FUNCTION.                <FA1971>
                                                                        <FA1971>
           PERFORM D3000-CALL-ENRLIO.                                   <FA1971>
                                                                        <FA1971>
           MOVE NEXTR                  TO ENRL-FUNCTION.                <FA1971>
           PERFORM D3000-CALL-ENRLIO.                                   <FA1971>
                                                                        <FA1971>
       A5290-EXIT.                                                      <FA1971>
           EXIT.                                                        <FA1971>
      /                                                                 <V75F01>
      ******************************                                    <V75F01>
       A5300-UPDATE-ENRLCLT SECTION.                                    <V75F01>
      ******************************                                    <V75F01>
       A5310-START.                                                     <V75F01>
      *                                                                 <V75F01>
           MOVE SPACES                 TO ENRLCLT-PARAMS.               <V75F01>
           MOVE PRFX-CLNT              TO ENRLCLT-CLNTPFX.              <V75F01>
           MOVE WSSP-FSUCO             TO ENRLCLT-CLNTCOY.              <V75F01>
           MOVE S2465-CLNTNUM          TO ENRLCLT-CLNTNUM.              <V75F01>
           MOVE BEGN                   TO ENRLCLT-FUNCTION.             <V75F01>
      *                                                                 <V75F01>
       A5320-READ-ENRLCLT.                                              <V75F01>
      *                                                                 <V75F01>
           CALL 'ENRLCLTIO'            USING ENRLCLT-PARAMS.            <V75F01>
      *                                                                 <V75F01>
           IF  ENRLCLT-STATUZ          NOT = O-K                        <V75F01>
           AND ENRLCLT-STATUZ          NOT = ENDP                       <V75F01>
               MOVE ENRLCLT-PARAMS     TO ENRLCLT-PARAMS                <V75F01>
               MOVE ENRLCLT-STATUZ     TO ENRLCLT-STATUZ                <V75F01>
               PERFORM 600-FATAL-ERROR                                  <V75F01>
           END-IF.                                                      <V75F01>
      *                                                                 <V75F01>
           IF  ENRLCLT-STATUZ          = ENDP                           <V75F01>
           OR  ENRLCLT-CLNTPFX         NOT = PRFX-CLNT                  <V75F01>
           OR  ENRLCLT-CLNTCOY         NOT = WSSP-FSUCO                 <V75F01>
           OR  ENRLCLT-CLNTNUM         NOT = S2465-CLNTNUM              <V75F01>
               GO TO A5390-EXIT                                         <V75F01>
           END-IF.                                                      <V75F01>
      *                                                                 <V75F01>
           MOVE WSAA-CNV-SURNAME       TO ENRLCLT-SURNAME.              <V75F01>
           MOVE WSAA-CNV-GIVNAME       TO ENRLCLT-GIVNAME.              <V75F01>
           MOVE WSAA-CNV-CLNTNAME      TO ENRLCLT-CLNTNAME.             <V75F01>
           MOVE S2465-SECUITYNO        TO ENRLCLT-SECUITYNO.            <V75F01>
           MOVE WRITD                  TO ENRLCLT-FUNCTION.             <V75F01>
      *                                                                 <V75F01>
           CALL 'ENRLCLTIO'            USING ENRLCLT-PARAMS.            <V75F01>
      *                                                                 <V75F01>
           IF  ENRLCLT-STATUZ          NOT = O-K                        <V75F01>
               MOVE ENRLCLT-PARAMS     TO ENRLCLT-PARAMS                <V75F01>
               MOVE ENRLCLT-STATUZ     TO ENRLCLT-STATUZ                <V75F01>
               PERFORM 600-FATAL-ERROR                                  <V75F01>
           END-IF.                                                      <V75F01>
      *                                                                 <V75F01>
       A5380-NEXT-ENRLCLT.                                              <V75F01>
      *                                                                 <V75F01>
           MOVE NEXTR                  TO ENRLCLT-FUNCTION.             <V75F01>
           GO TO A5320-READ-ENRLCLT.                                    <V75F01>
      *                                                                 <V75F01>
       A5390-EXIT.                                                      <V75F01>
           EXIT.                                                        <V75F01>
      /                                                                 <V75F01>
      ****************************                                      <V75F01>
       A5400-CALL-BLDENRL SECTION.                                      <V75F01>
      ****************************                                      <V75F01>
      *                                                                 <V75F01>
       A5410-START.                                                     <V75F01>
      *                                                                 <V75F01>
           INITIALIZE                     BLDENRL-BLDENRLREC.           <V75F01>
           MOVE PRFX-CLNT              TO BLDENRL-PREFIX.               <V75F01>
           MOVE WSSP-FSUCO             TO BLDENRL-COMPANY.              <V75F01>
           MOVE S2465-CLNTNUM          TO BLDENRL-UENTITY.              <V75F01>
           MOVE WSSP-USERID            TO BLDENRL-USERID.               <V75F01>
      *                                                                 <V75F01>
           CALL 'BLDENRL'              USING BLDENRL-BLDENRLREC.        <V75F01>
      *                                                                 <V75F01>
           IF  BLDENRL-STATUZ          NOT = O-K                        <V75F01>
               MOVE BLDENRL-BLDENRLREC TO SYSR-PARAMS                   <V75F01>
               MOVE BLDENRL-STATUZ     TO SYSR-STATUZ                   <V75F01>
               PERFORM 600-FATAL-ERROR                                  <V75F01>
           END-IF.                                                      <V75F01>
      *                                                                 <V75F01>
       A5490-EXIT.                                                      <V75F01>
           EXIT.                                                        <V75F01>
      /                                                                 <FA1971>
       A6000-CALL-MSGBOX SECTION.                                       <V62P07>
      ***************************                                       <V62P07>
       A6010-BEGIN.                                                     <V62P07>
      *                                                                 <V62P07>
           MOVE SPACES                 TO MBOX-CPFMSG MBOX-INSERT       <V62P07>
                                          MBOX-REPLY  MBOX-RESULT.      <V62P07>
           MOVE WSSP-LANGUAGE          TO WSAA-LANGUAGE-MSG.            <V62P07>
           MOVE '0006'                 TO WSAA-MSGID.                   <V62P07>
           MOVE WSAA-PAXMSG            TO MBOX-CPFMSG.                  <V62P07>
           MOVE 'X'                    TO MBOX-REPLY.                   <V62P07>
                                                                        <V62P07>
           CALL 'MSGBOX'            USING MBOX-CPFMSG  MBOX-INSERT      <V62P07>
                                          MBOX-REPLY   MBOX-RESULT.     <V62P07>
                                                                        <V62P07>
       A6090-EXIT.                                                      <V62P07>
           EXIT.                                                        <V62P07>
      /                                                                 <V62P07>
       A7000-CHECK-SOIN SECTION.                                        <FA4547>
      ***************************                                       <FA4547>
       A7010-BEGIN.                                                     <FA4547>
                                                                        <FA4547>
           MOVE WSSP-FSUCO             TO SOIN-CLNTCOY.                 <FA4547>
           MOVE S2465-CLNTNUM          TO SOIN-CLNTNUM.                 <FA4547>
           MOVE ZERO                   TO SOIN-INCOME-SEQ-NO.           <FA4547>
           MOVE BEGN                   TO SOIN-FUNCTION.                <FA4547>
           MOVE SOINREC                TO SOIN-FORMAT.                  <FA4547>
           CALL 'SOINIO'            USING SOIN-PARAMS.                  <FA4547>
           IF SOIN-STATUZ           NOT = O-K AND ENDP                  <FA4547>
               MOVE SOIN-PARAMS        TO SYSR-PARAMS                   <FA4547>
               PERFORM 600-FATAL-ERROR                                  <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
           IF SOIN-STATUZ               = ENDP                          <FA4547>
           OR SOIN-CLNTCOY          NOT = WSSP-FSUCO                    <FA4547>
           OR SOIN-CLNTNUM          NOT = S2465-CLNTNUM                 <FA4547>
               MOVE SPACE              TO S2465-UK-PENSION-IND          <FA4547>
           ELSE                                                         <FA4547>
               MOVE '+'                TO S2465-UK-PENSION-IND          <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
       A7090-EXIT.                                                      <FA4547>
           EXIT.                                                        <FA4547>
      /                                                                 <FA4547>
       A8000-CHECK-RACR SECTION.                                        <FA4547>
      ***************************                                       <FA4547>
       A8010-BEGIN.                                                     <FA4547>
                                                                        <FA4547>
           MOVE SPACES                 TO RACR-PARAMS.                  <FA4547>
           MOVE SPACES                 TO S2465-RACRIND.                <FA4547>
                                                                        <FA4547>
           MOVE WSSP-FSUCO             TO RACR-CLNTCOY.                 <FA4547>
           MOVE CLTS-CLNTPFX           TO RACR-CLNTPFX.                 <FA4547>
           MOVE CLTS-CLNTNUM           TO RACR-CLNTNUM.                 <FA4547>
           MOVE SPACES                 TO RACR-LRKCLS.                  <FA4547>
           MOVE ZEROES                 TO RACR-CURRFROM.                <FA4547>
                                                                        <FA4547>
           MOVE BEGN                   TO RACR-FUNCTION.                <FA4547>
                                                                        <FA4547>
           PERFORM 2200-CHECK-FOR-RACR                                  <FA4547>
                      UNTIL RACR-STATUZ = ENDP.                         <FA4547>
                                                                        <FA4547>
       A8090-EXIT.                                                      <FA4547>
           EXIT.                                                        <FA4547>
      /                                                                 <FA4547>
       A9000-CHECK-BNKRPT SECTION.                                      <FA4547>
      ***************************                                       <FA4547>
       A9010-BEGIN.                                                     <FA4547>
                                                                        <FA4547>
           INITIALIZE                     BRUP-PARAMS.                  <FA4547>
           MOVE CLTS-CLNTCOY           TO BRUP-CLNTCOY.                 <FA4547>
           MOVE CLTS-CLNTNUM           TO BRUP-CLNTNUM.                 <FA4547>
           MOVE ZERO                   TO BRUP-BRUPDTE.                 <FA4547>
           MOVE BEGN                   TO BRUP-FUNCTION.                <FA4547>
           MOVE BRUPREC                TO BRUP-FORMAT.                  <FA4547>
                                                                        <FA4547>
           CALL 'BRUPIO'            USING BRUP-PARAMS.                  <FA4547>
                                                                        <FA4547>
           IF BRUP-STATUZ           NOT = O-K                           <FA4547>
           AND                            ENDP                          <FA4547>
               MOVE BRUP-STATUZ        TO SYSR-STATUZ                   <FA4547>
               MOVE BRUP-PARAMS        TO SYSR-PARAMS                   <FA4547>
               PERFORM 600-FATAL-ERROR                                  <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
           IF BRUP-STATUZ               = ENDP                          <FA4547>
           OR BRUP-CLNTCOY          NOT = CLTS-CLNTCOY                  <FA4547>
           OR BRUP-CLNTNUM          NOT = CLTS-CLNTNUM                  <FA4547>
               MOVE SPACE              TO S2465-BRUPIND                 <FA4547>
           ELSE                                                         <FA4547>
               MOVE '+'                TO S2465-BRUPIND                 <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
       A9090-EXIT.                                                      <FA4547>
           EXIT.                                                        <FA4547>
      /                                                                 <FA4547>
       A10000-CHECK-CLPR  SECTION.                                      <FA4547>
      ***************************                                       <FA4547>
       A10010-BEGIN.                                                    <FA4547>
      * Check Client Profiling Indicator                                <FA4547>
           INITIALIZE                     CLPR-PARAMS.                  <FA4547>
           MOVE CLTS-CLNTPFX           TO CLPR-CLNTPFX.                 <FA4547>
           MOVE CLTS-CLNTCOY           TO CLPR-CLNTCOY.                 <FA4547>
           MOVE CLTS-CLNTNUM           TO CLPR-CLNTNUM.                 <FA4547>
           MOVE READR                  TO CLPR-FUNCTION.                <FA4547>
           MOVE CLPRREC                TO CLPR-FORMAT.                  <FA4547>
                                                                        <FA4547>
           CALL 'CLPRIO'            USING CLPR-PARAMS.                  <FA4547>
                                                                        <FA4547>
           IF CLPR-STATUZ           NOT = O-K AND MRNF                  <FA4547>
               MOVE CLPR-STATUZ        TO SYSR-STATUZ                   <FA4547>
               MOVE CLPR-PARAMS        TO SYSR-PARAMS                   <FA4547>
               PERFORM 600-FATAL-ERROR                                  <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
           IF CLPR-STATUZ               = MRNF                          <FA4547>
              MOVE SPACE               TO S2465-CLPRFIND                <FA4547>
           ELSE                                                         <FA4547>
              MOVE '+'                 TO S2465-CLPRFIND                <FA4547>
           END-IF.                                                      <FA4547>
                                                                        <FA4547>
       A10090-EXIT.                                                     <FA4547>
           EXIT.                                                        <FA4547>
      /                                                                 <FA4547>
       D1000-DELETE-CLEX SECTION.                                       <CN005>
      ***************************                                       <CN005>
       D1100-DELETE-CLEX.                                               <CN005>
      *                                                                 <CN005>
           MOVE SPACES                 TO CLEX-PARAMS.                  <CN005>
           MOVE WSSP-CLNTKEY           TO CLEX-DATA-KEY.                <CN005>
           MOVE CLEXREC                TO CLEX-FORMAT.                  <CN005>
           MOVE READH                  TO CLEX-FUNCTION.                <CN005>
           CALL 'CLEXIO'                 USING CLEX-PARAMS.             <CN005>
           IF CLEX-STATUZ              NOT = O-K AND MRNF               <CN005>
               MOVE CLEX-PARAMS        TO SYSR-PARAMS                   <CN005>
               PERFORM 600-FATAL-ERROR.                                 <CN005>
                                                                        <CN005>
           IF CLEX-STATUZ = O-K                                         <CN005>
               MOVE CLEXREC            TO CLEX-FORMAT                   <CN005>
      ****     MOVE DELET              TO CLEX-FUNCTION         <V73F02><CN005>
               MOVE '2'                TO CLEX-VALIDFLAG                <V73F02>
               MOVE REWRT              TO CLEX-FUNCTION                 <V73F02>
               CALL 'CLEXIO'           USING CLEX-PARAMS                <CN005>
               IF CLEX-STATUZ          NOT = O-K                        <CN005>
                  MOVE CLEX-PARAMS     TO SYSR-PARAMS                   <CN005>
                  PERFORM 600-FATAL-ERROR                               <CN005>
               END-IF                                                   <CN005>
           END-IF.                                                      <CN005>
                                                                        <CN005>
       D1900-EXIT.                                                      <CN005>
            EXIT.                                                       <CN005>
      /                                                                 <PSE309>
       D2000-DELETE-RACR SECTION.                                       <PSE309>
      ***************************                                       <PSE309>
       D2100-DELETE-RACR.                                               <PSE309>
      *                                                                 <PSE309>
           MOVE SPACES                 TO RACR-PARAMS.                  <PSE309>
           MOVE WSSP-CLNTKEY           TO RACR-DATA-KEY.                <PSE309>
           MOVE VRCM-MAX-DATE          TO RACR-CURRFROM.                <PSE309>
           MOVE RACRREC                TO RACR-FORMAT.                  <PSE309>
           MOVE BEGNH                  TO RACR-FUNCTION.                <PSE309>
                                                                        <PSE309>
       D2200-LOOP.                                                      <PSE309>
                                                                        <PSE309>
           CALL 'RACRIO'                 USING RACR-PARAMS.             <PSE309>
           IF RACR-STATUZ              NOT = O-K AND ENDP               <PSE309>
               MOVE RACR-PARAMS        TO SYSR-PARAMS                   <PSE309>
               PERFORM 600-FATAL-ERROR.                                 <PSE309>
                                                                        <PSE309>
           IF RACR-STATUZ = O-K                                         <PSE309>
               IF RACR-CLNTPFX         = PRFX-CLNT AND                  <PSE309>
                  RACR-CLNTCOY         = WSSP-FSUCO AND                 <PSE309>
                  RACR-CLNTNUM         = WSSP-CLNTKEY(4:8)              <PSE309>
                  MOVE RACRREC         TO RACR-FORMAT                   <PSE309>
      ****        MOVE DELET           TO RACR-FUNCTION         <V73F02><PSE309>
                  MOVE '2'             TO RACR-VALIDFLAG                <V73F02>
                  MOVE REWRT           TO RACR-FUNCTION                 <V73F02>
                  CALL 'RACRIO'        USING RACR-PARAMS                <PSE309>
                  IF RACR-STATUZ       NOT = O-K                        <PSE309>
                     MOVE RACR-PARAMS  TO SYSR-PARAMS                   <PSE309>
                     PERFORM 600-FATAL-ERROR                            <PSE309>
                  END-IF                                                <PSE309>
               ELSE                                                     <PSE309>
                  GO TO D2900-EXIT                                      <PSE309>
               END-IF                                                   <PSE309>
           ELSE                                                         <PSE309>
               GO TO D2900-EXIT                                         <PSE309>
           END-IF.                                                      <PSE309>
                                                                        <PSE309>
           MOVE NEXTR                  TO RACR-FUNCTION.                <PSE309>
           GO TO D2200-LOOP.                                            <PSE309>
                                                                        <PSE309>
       D2900-EXIT.                                                      <PSE309>
            EXIT.                                                       <PSE309>

       D3000-CALL-ENRLIO SECTION.                                       <FA1971>
      ***************************                                       <FA1971>
       D3010-ENRLIO.                                                    <FA1971>

           MOVE ENRLREC                TO ENRL-FORMAT.                  <FA1971>
           CALL 'ENRLIO'            USING ENRL-PARAMS.                  <FA1971>

           IF ENRL-STATUZ           NOT = O-K  AND ENDP AND MRNF        <FA1971>
              MOVE ENRL-PARAMS         TO SYSR-PARAMS                   <FA1971>
              MOVE ENRL-STATUZ         TO SYSR-STATUZ                   <FA1971>
              PERFORM 600-FATAL-ERROR                                   <FA1971>
           END-IF.                                                      <FA1971>

       D3090-EXIT.                                                      <FA1971>
           EXIT.                                                        <FA1971>
      /                                                                 <V71L02>
       D4000-DELETE-BRUP SECTION.                                       <V71L02>
      ***************************                                       <V71L02>
       D4100-DELETE-BRUP.                                               <V71L02>
      *                                                                 <V71L02>
           INITIALIZE                     BRUP-PARAMS.                  <V71L02>
           MOVE CLTS-CLNTCOY           TO BRUP-CLNTCOY.                 <V71L02>
           MOVE CLTS-CLNTNUM           TO BRUP-CLNTNUM.                 <V71L02>
           MOVE ZERO                   TO BRUP-BRUPDTE.                 <V71L02>
           MOVE BRUPREC                TO BRUP-FORMAT.                  <V71L02>
           MOVE BEGNH                  TO BRUP-FUNCTION.                <V71L02>
                                                                        <V71L02>
       D4200-LOOP.                                                      <V71L02>
                                                                        <V71L02>
           CALL 'BRUPIO'               USING BRUP-PARAMS.               <V71L02>
           IF BRUP-STATUZ              NOT = O-K AND ENDP               <V71L02>
               MOVE BRUP-PARAMS        TO SYSR-PARAMS                   <V71L02>
               PERFORM 600-FATAL-ERROR.                                 <V71L02>
                                                                        <V71L02>
           IF BRUP-STATUZ = O-K                                         <V71L02>
               IF BRUP-CLNTCOY         = CLTS-CLNTCOY AND               <V71L02>
                  BRUP-CLNTNUM         = CLTS-CLNTNUM                   <V71L02>
                  MOVE BRUPREC         TO BRUP-FORMAT                   <V71L02>
      ****        MOVE DELET           TO BRUP-FUNCTION         <V73F02><V71L02>
                  MOVE '2'             TO BRUP-VALIDFLAG                <V73F02>
                  MOVE REWRT           TO BRUP-FUNCTION                 <V73F02>
                  CALL 'BRUPIO'        USING BRUP-PARAMS                <V71L02>
                  IF BRUP-STATUZ       NOT = O-K                        <V71L02>
                     MOVE BRUP-PARAMS  TO SYSR-PARAMS                   <V71L02>
                     PERFORM 600-FATAL-ERROR                            <V71L02>
                  END-IF                                                <V71L02>
               ELSE                                                     <V71L02>
                  GO TO D4900-EXIT                                      <V71L02>
               END-IF                                                   <V71L02>
           ELSE                                                         <V71L02>
              GO TO D4900-EXIT                                          <V71L02>
           END-IF.                                                      <V71L02>
                                                                        <V71L02>
           MOVE NEXTR                  TO BRUP-FUNCTION.                <V71L02>
           GO TO D4200-LOOP.                                            <V71L02>
                                                                        <V71L02>
        D4900-EXIT.                                                     <V71L02>
             EXIT.                                                      <V71L02>
      *
       D5000-DELETE-CLPR SECTION.                                       <V74F03>
      ***************************                                       <V74F03>
       D5000-BEGIN.                                                     <V74F03>
                                                                        <V74F03>
           MOVE WSSP-CLNTKEY           TO WSAA-CLPRKEY.                 <V74F03>
           INITIALIZE                     CLPR-PARAMS.                  <V74F03>
           MOVE WSKY-CLPR-CLNTPFX      TO CLPR-CLNTPFX.                 <V74F03>
           MOVE WSKY-CLPR-CLNTCOY      TO CLPR-CLNTCOY.                 <V74F03>
           MOVE WSKY-CLPR-CLNTNUM      TO CLPR-CLNTNUM.                 <V74F03>
           MOVE CLPRREC                TO CLPR-FORMAT.                  <V74F03>
           MOVE READH                  TO CLPR-FUNCTION.                <V74F03>
           CALL 'CLPRIO'            USING CLPR-PARAMS.                  <V74F03>
                                                                        <V74F03>
           IF CLPR-STATUZ           NOT = O-K AND MRNF                  <V74F03>
              MOVE CLPR-STATUZ         TO SYSR-STATUZ                   <V74F03>
              MOVE CLPR-PARAMS         TO SYSR-PARAMS                   <V74F03>
              PERFORM 600-FATAL-ERROR                                   <V74F03>
           END-IF.                                                      <V74F03>
                                                                        <V74F03>
           IF CLPR-STATUZ               = O-K                           <V74F03>
              MOVE '2'                 TO CLPR-VALIDFLAG                <V74F03>
              MOVE CLPR-USER-PROFILE   TO CLPR-CRTUSER                  <V74F03>
              MOVE REWRT               TO CLPR-FUNCTION                 <V74F03>
              MOVE CLPRREC             TO CLPR-FORMAT                   <V74F03>
              CALL 'CLPRIO'            USING CLPR-PARAMS                <V74F03>
                                                                        <V74F03>
              IF CLPR-STATUZ           NOT = O-K AND MRNF               <V74F03>
                 MOVE CLPR-STATUZ      TO SYSR-STATUZ                   <V74F03>
                 MOVE CLPR-PARAMS      TO SYSR-PARAMS                   <V74F03>
                 PERFORM 600-FATAL-ERROR                                <V74F03>
              END-IF                                                    <V74F03>
           END-IF.                                                      <V74F03>
      *                                                                 <V74F03>
       D5000-EXIT.                                                      <V74F03>
            EXIT.                                                       <V74F03>
      /                                                                 <V74F03>
       A5600-RETN-ALOCNO   SECTION.                                     <FA5288>
      *****************************                                     <FA5288>
       A5610-BEGIN.                                                     <FA5288>
           INITIALIZE                     CLTSREC-KEY-DATA.             <FA5288>
           MOVE WSSP-CLNTKEY           TO CLTS-DATA-KEY.                <FA5288>
           MOVE READR                  TO CLTS-FUNCTION.                <FA5288>
           MOVE CLTSREC                TO CLTS-FORMAT.                  <FA5288>
           CALL 'CLTSIO'            USING CLTS-PARAMS.                  <FA5288>
           IF CLTS-STATUZ           NOT = O-K AND MRNF                  <FA5288>
               MOVE CLTS-PARAMS        TO SYSR-PARAMS                   <FA5288>
               MOVE CLTS-STATUZ        TO SYSR-STATUZ                   <FA5288>
               PERFORM 600-FATAL-ERROR                                  <FA5288>
           END-IF.                                                      <FA5288>
           IF CLTS-STATUZ               = O-K                           <FA5288>
              GO TO A5690-EXIT.                                         <FA5288>
                                                                        <FA5288>
           INITIALIZE                     ALNO-ALOCNO-REC.              <FA5288>
           MOVE 'RETN'                 TO ALNO-FUNCTION.                <FA5288>
           MOVE PRFX-CLNT              TO ALNO-PREFIX.                  <FA5288>
           MOVE WSSP-FSUCO             TO ALNO-COMPANY.                 <FA5288>
           MOVE WSSP-BRANCH            TO ALNO-GENKEY.                  <FA5288>
           MOVE WSSP-CLNTKEY           TO WSAA-CLTSKEY.                 <FA5288>
           MOVE WSKY-CLTS-CLNTNUM      OF WSAA-CLTSKEY                  <FA5288>
                                       TO ALNO-ALOC-NO.                 <FA5288>
                                                                        <FA5288>
           CALL 'ALOCNO' USING ALNO-ALOCNO-REC.                         <FA5288>
           IF ALNO-STATUZ               = BOMB                          <FA5288>
              MOVE ALNO-ALOCNO-REC     TO SYSR-PARAMS                   <FA5288>
              MOVE ALNO-STATUZ         TO SYSR-STATUZ                   <FA5288>
              PERFORM 600-FATAL-ERROR                                   <FA5288>
           END-IF.                                                      <FA5288>
           INITIALIZE                  ALNO-ALOCNO-REC.                 <FA5288>
       A5690-EXIT.                                                      <FA5288>
           EXIT.                                                        <FA5288>
      /                                                                 <GAPPH2>
       7000-WRITE-LETTER SECTION.                                       <GAPPH2>
      ***************************                                       <GAPPH2>
      *                                                                 <GAPPH2>
       7010-READ-T6634.                                                 <GAPPH2>
      *                                                                 <GAPPH2>
           IF  WSSP-FLAG            NOT = 'M'                           <GAPPH2>
      **** OR  S2465-DATA-FIELDS        = WSAA-CLNT-DATA-SAVE   <PHFX02><GAPPH2>
               GO TO 7099-EXIT                                          <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <PHFX02>
           IF  S2465-LSURNAME           = WSAA-LSURNAME-SAV             <PHFX02>
           AND S2465-LGIVNAME           = WSAA-LGIVNAME-SAV             <PHFX02>
           AND S2465-SECUITYNO          = WSAA-SECUITYNO-SAV            <PHFX02>
           AND S2465-CLTDOBX            = WSAA-CLTDOBX-SAV              <PHFX02>
           AND S2465-CLTSEX             = WSAA-CLTSEX-SAV               <PHFX02>
           AND S2465-CTRYCODE           = WSAA-CTRYCODE-SAV             <PHFX02>
      *    AND S2465-RMBLPHONE          = WSAA-RMBLPHONE-SAV    <PS012> <PHFX02>
      *    AND S2465-RINTERNET          = WSAA-RINTERNET-SAV    <PS012> <PHFX02>
           AND S2465-SOE                = WSAA-SOE-SAV                  <PS012>
           AND S2465-IDDATE             = WSAA-IDDATE-SAV               <PS012>
           AND S2465-IDPLACE            = WSAA-IDPLACE-SAV              <PS012>
           AND S2465-IDPLACEXT          = WSAA-IDPLACEXT-SAV            <CLM14>
               GO TO 7099-EXIT                                          <PHFX02>
           END-IF.                                                      <PHFX02>
      *                                                                 <PHFX02>
           IF  S2465-UK-PENSION-IND     = 'X'                           <PHFX02>
           OR  S2465-TAXFLAG            = 'X'                           <PHFX02>
           OR  S2465-RACRIND            = 'X'                           <PHFX02>
           OR  S2465-BRUPIND            = 'X'                           <PHFX02>
           OR  S2465-REXTRFLD           = 'X'                           <PHFX02>
           OR  S2465-CLPRFIND           = 'X'                           <PHFX02>
               GO TO 7099-EXIT                                          <PHFX02>
           END-IF.                                                      <PHFX02>
      *                                                                 <PHFX02>
      *    IF  S2465-TAXFLAG        NOT = WSAA-TAXFLAG-SAV      <PS012> <PHFX02>
      *    OR  S2465-RACRIND        NOT = WSAA-RACRIND-SAV      <PS012> <PHFX02>
      *    OR  S2465-BRUPIND        NOT = WSAA-BRUPIND-SAV      <PS012> <PHFX02>
      *    OR  S2465-REXTRFLD       NOT = WSAA-REXTRFLD-SAV     <PS012> <PHFX02>
      *    OR  S2465-CLPRFIND       NOT = WSAA-CLPRFIND-SAV     <PS012> <PHFX02>
      *        GO TO 7099-EXIT                                  <PS012> <PHFX02>
      *    END-IF.                                              <PS012> <PHFX02>
      *                                                                 <GAPPH2>
      *  Get the Letter type from T6634.                                <GAPPH2>
      *                                                                 <GAPPH2>
           MOVE SPACES                     TO ITEM-DATA-AREA.           <GAPPH2>
           MOVE 'IT'                       TO ITEM-ITEMPFX.             <GAPPH2>
           MOVE '2'                        TO ITEM-ITEMCOY.             <GAPPH2>
           MOVE TR384                      TO ITEM-ITEMTABL.            <GAPPH2>
      *                                                                 <GAPPH2>
      *  Build key to T6634 from transaction code                       <GAPPH2>
      *                                                                 <GAPPH2>
           STRING '***'                                                 <GAPPH2>
                  WSKY-BATC-BATCTRCDE                                   <GAPPH2>
                  DELIMITED BY SPACE                                    <GAPPH2>
                       INTO ITEM-ITEMITEM                               <GAPPH2>
           END-STRING.                                                  <GAPPH2>
                                                                        <GAPPH2>
           MOVE READR                  TO ITEM-FUNCTION.                <GAPPH2>
                                                                        <GAPPH2>
           CALL 'ITEMIO'            USING ITEM-PARAMS.                  <GAPPH2>
                                                                        <GAPPH2>
           IF  ITEM-STATUZ          NOT = O-K  AND  MRNF                <GAPPH2>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <GAPPH2>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <GAPPH2>
               PERFORM 600-FATAL-ERROR                                  <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
           IF  ITEM-STATUZ              = MRNF                          <GAPPH2>
               MOVE SPACES             TO ITEM-GENAREA                  <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
      *                                                                 <GAPPH2>
           MOVE ITEM-GENAREA               TO TR384-TR384-REC.          <GAPPH2>
      *                                                                 <GAPPH2>
           IF  TR384-LETTER-TYPE           = SPACES                     <GAPPH2>
               GO TO 7099-EXIT.                                         <GAPPH2>
      *                                                                 <GAPPH2>
      *  Write LETC via LETRQST and update the seqno by 1               <GAPPH2>
      *                                                                 <GAPPH2>
           MOVE SPACES                 TO LETRQST-STATUZ.               <GAPPH2>
           MOVE WSKY-BATC-BATCCOY      TO LETRQST-REQUEST-COMPANY.      <GAPPH2>
           MOVE TR384-LETTER-TYPE      TO LETRQST-LETTER-TYPE.          <GAPPH2>
           MOVE WSKY-CLTS-CLNTCOY      TO LETRQST-CLNTCOY.              <GAPPH2>
           MOVE WSKY-CLTS-CLNTNUM      TO LETRQST-CLNTNUM.              <GAPPH2>
           MOVE WSAA-TODAY             TO LETRQST-LETTER-REQUEST-DATE.  <GAPPH2>
           MOVE WSKY-CLTS-CLNTPFX      TO LETRQST-RDOCPFX.              <GAPPH2>
           MOVE WSKY-CLTS-CLNTCOY      TO LETRQST-RDOCCOY.              <GAPPH2>
           MOVE WSKY-CLTS-CLNTNUM      TO LETRQST-RDOCNUM.              <GAPPH2>
           MOVE SPACES                 TO LETRQST-CHDRCOY.              <GAPPH2>
           MOVE SPACES                 TO LETRQST-CHDRNUM.              <GAPPH2>
  *********MOVE SPACES                 TO LETRQST-TRANNO.       <DA002> <GAPPH2>
           MOVE '0'                    TO LETRQST-TRANNO.               <DA002>
           MOVE WSSP-BRANCH            TO LETRQST-BRANCH.               <GAPPH2>
           MOVE SPACES                 TO LETRQST-DESPNUM.              <GAPPH2>
           MOVE WSKY-BATC-BATCTRCDE    TO LETRQST-TRCDE.                <GAPPH2>
           MOVE WSKY-BATC-BATCTRCDE    TO LETRQST-OTHER-KEYS.           <GAPPH2>
           STRING                          WSKY-CLTS-CLNTNUM            <GAPPH2>
                                           WSKY-BATC-BATCTRCDE          <GAPPH2>
                                           WSSP-LANGUAGE                <GAPPH2>
           DELIMITED BY                    SPACE                        <GAPPH2>
           INTO                            LETRQST-OTHER-KEYS.          <GAPPH2>
                                                                        <GAPPH2>
           MOVE 'ADD'                  TO LETRQST-FUNCTION.             <GAPPH2>
      *                                                                 <GAPPH2>
           IF  TR384-LETTER-TYPE       NOT = SPACES                     <GAPPH2>
      *                                                                 <GAPPH2>
               CALL 'LETRQST' USING LETRQST-PARAMS                      <GAPPH2>
      *                                                                 <GAPPH2>
               IF  LETRQST-STATUZ          NOT = O-K                    <GAPPH2>
                   MOVE LETRQST-PARAMS     TO SYSR-PARAMS               <GAPPH2>
                   MOVE LETRQST-STATUZ     TO SYSR-STATUZ               <GAPPH2>
                   PERFORM 600-FATAL-ERROR                              <GAPPH2>
               END-IF                                                   <GAPPH2>
           END-IF.                                                      <GAPPH2>
                                                                        <GAPPH2>
       7099-EXIT.                                                       <GAPPH2>
           EXIT.                                                        <GAPPH2>
      /                                                                 <NB019>
       X1000-CHECK-ADDRESS-DUP SECTION.                                 <NB019>
      *********************************                                 <NB019>
       X1001-START.                                                     <NB019>
      *                                                                 <NB019>
           MOVE 'N'                    TO WSAA-ADDRESS-DUP.             <NB019>
                                                                        <NB019>
           MOVE S2465-ZPTCITY          TO WSAA-CITY.                    <NB019>
           MOVE S2465-ZPTDIST          TO WSAA-DIST.                    <NB019>
           MOVE S2465-ZPTWARD          TO WSAA-WARD.                    <NB019>
                                                                        <NB019>
           INITIALIZE                  CLNTCHK-PARAMS.                  <NB019>
           MOVE WSAA-LOCCODE           TO CLNTCHK-CLTPCODE.             <NB019>
           MOVE SPACES                 TO CLNTCHK-CLTADDR01.            <NB019>
           MOVE SPACES                 TO CLNTCHK-CLTADDR02.            <NB019>
           MOVE CLNTCHKREC             TO CLNTCHK-FORMAT.               <NB019>
           MOVE BEGN                   TO CLNTCHK-FUNCTION.             <NB019>
                                                                        <NB019>
       X1010-CALLIO.                                                    <NB019>
                                                                        <NB019>
           CALL 'CLNTCHKIO'            USING CLNTCHK-PARAMS.            <NB019>
                                                                        <NB019>
           IF  CLNTCHK-STATUZ          NOT = O-K                        <NB019>
           AND                         NOT = ENDP                       <NB019>
               MOVE CLNTCHK-PARAMS     TO SYSR-PARAMS                   <NB019>
               PERFORM 600-FATAL-ERROR                                  <NB019>
           END-IF.                                                      <NB019>
                                                                        <NB019>
           IF  CLNTCHK-STATUZ          NOT = O-K                        <NB019>
           OR  CLNTCHK-CLTPCODE        NOT = WSAA-LOCCODE               <NB019>
               GO TO X1019-EXIT                                         <NB019>
           END-IF.                                                      <NB019>
                                                                        <NB019>
      * Skip if Client Code = S2465-CLNTNUM                             <NB019>
                                                                        <NB019>
           IF  CLNTCHK-CLNTNUM         = S2465-CLNTNUM                  <NB019>
               GO TO X1018-NEXTR                                        <NB019>
           END-IF.                                                      <NB019>
                                                                        <NB019>
           IF  CLNTCHK-STATUZ          = O-K                            <NB019>
           AND CLNTCHK-CLTPCODE        = WSAA-LOCCODE                   <NB019>
               MOVE SPACES             TO WSAA-ADDRLINE12               <NB019>
               MOVE SPACES             TO WSAA-CLNTLINE12               <NB019>
               STRING                                                   <NB019>
                   S2465-CLTADDR (1)   DELIMITED BY '  '                <NB019>
                   ' '                 DELIMITED BY SIZE                <NB019>
                   S2465-CLTADDR (2)   DELIMITED BY '  '                <NB019>
                                       INTO WSAA-ADDRLINE12             <NB019>
               END-STRING                                               <NB019>
               STRING                                                   <NB019>
                   CLNTCHK-CLTADDR01   DELIMITED BY '  '                <NB019>
                   ' '                 DELIMITED BY SIZE                <NB019>
                   CLNTCHK-CLTADDR02   DELIMITED BY '  '                <NB019>
                                       INTO WSAA-CLNTLINE12             <NB019>
               END-STRING                                               <NB019>
               IF WSAA-ADDRLINE12      = WSAA-CLNTLINE12                <NB019>
                   MOVE 'Y'            TO WSAA-ADDRESS-DUP              <NB019>
                   GO TO X1019-EXIT                                     <NB019>
               END-IF                                                   <NB019>
           END-IF.                                                      <NB019>
                                                                        <NB019>
       X1018-NEXTR.                                                     <NB019>
           MOVE NEXTR                  TO CLNTCHK-FUNCTION.             <NB019>
           GO TO X1010-CALLIO.                                          <NB019>
      *                                                                 <NB019>
       X1019-EXIT.                                                      <NB019>
           EXIT.                                                        <NB019>
