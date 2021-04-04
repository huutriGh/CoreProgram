      * Generation Parameters - SCRVER(02)            Do Not Delete|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. P2610.
      **
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *
      * RECEIPT DISSECTION
      * ==================
      *
      * Create Cash Not Banked/Create Banked Cash/Cash Enquiry
      * 
      *
      * This FSU program has 2 modes:
      *  - Create (identical for Cash Not Banked and Banked Cash)
      *  - Enquiry.
      *
      * The header of S2610 contains information about the Receipt and
      * the subfile allows multi-dissection.
      *
      * There are up to 999 dissection lines available below the header.
      * Each dissection line contains the SACSCODE and SACSTYP which
      * define the type of dissection.
      * For each SACSCODE, an optional editing program can be defined
      * in T3697.  This editing routine can perform any special checks
      * that are deemed necessary for the receipting program.
      *
      * In addition, a special update routine can also be defined on
      * T3697. It follows that any special updating can be done from
      * within that routine.
      *
      * Should a dissection require a special Pop-Up screen, then it is
      * possible to add one without altering this program.  You need to
      * define valid selections within the edit routines.  The edit
      * routine will pass back to this program a code held in the INDIC
      * field.  This INDIC is used in OPTSWCH to identify the correct
      * PopUp program to call.
      *
      * For partial receipt cancellation, user may choose which
      * dissection details to be cancelled. User is allowed to change
      * the Original Amount and Local Amount only as long as the
      * total of these two amount fields are tied up with the total
      * amount from Receipt Header screen (i.e first dissection line
      * in this program).
      *
      * User can press function key F13 to switch to Receipt Header
      * screen to make necessary changes on the informations.
      *
      * Note: there is some validation for the Enquiry mode, as the
      * user may make selections.
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      *
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      * 11/07/01    CCT    Version 2 - Old program P2610 is splitted        *
      * V6F108             into two programs PR289 and P2610                *
      *
      * 01/08/01    LSF    Modify to cater inter branch processing where    *
      * V6F114             GL branch substitution for bank will follow      *
      *                    sign on branch whereas GL branch for policy      *
      *                    dissection will follow contract branch.          *
      *
      * 03/09/01    YONGYUT                                                 *
      *                    To move agent key into RTRN if selection is 'AG' *
      *                                                                     *
      * 20/09/01    YUWADEE                                                 *
      *                    When Fund/Terr.Code not match should be          *
      *                    highlight at Dissection key field instead of     *
      *                    Sacs Code & Type.                                *
      *                                                                     *
      * 21/09/01    DARYLL                                                  *
      *                    No longer perform 3400 section which involves    *
      *                    the moving of "irrelevant data" into             *
      *                    RDOC-TRANDESC. This conflicts the printout       *
      *                    of the recepient's name.                         *
      *                                                                     *
      * 25/09/01    R.Nagamuthu                                             *
      * FA2050             Check WSSP-NO-OF-SUB whether it is numeric       *
      *                    before proceed to the next step.                 *
      *                                                                     *
      * 28/09/01    Kok Chiew Ping                                     NE   *
      * V62P04             Additional KFK Fields in the 2640-EDIT-ROUTINE   *
      *                    and 3512-CALL-UPDATE-ROUTINE section.            *
      *                                                                     *
      * 29/09/01    David Ong                                               *
      * V62P04             For Receipt Cancellation, it is always using     *
      *                    the last Tranno from claim. It should use the    *
      *                    Tranno during cancellation.                      *
      *                                                                     *
      * 01/10/01    R.Nagamuthu                                             *
      * R2-292             If Inter Branch checking is applicable display   *
      *                    warning message for the first time only.         *
      *                                                                     *
      * 04/10/01    David Ong                                               *
      * R2-296           - Change to use SRNCH because if user changes a    *
      *                    subfile record, then it may be mandatory to      *
      *                    revisit the popup. Whether it is mandatory or    *
      *                    not, it is decided by T3697-VALPROG.             *
      *                  - Due to change to use SRNCH, change accumulation  *
      *                    logic as well to check for imbalance.            *
      *                  - Also due to SRNCH, change logic checking on      *
      *                    Number of Dissection count.                      *
      *                  - Also due to SRNCH, change logic checking on      *
      *                    whether switching required or not.               *
      *                                                                     *
      ***********************************************************************
      /
      ***********************************************************************
      *                                                                     *
      * ......... New Version of the Amendment History.                     *
      *                                                                     *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      * 15/10/01  01/01   FA2088       David Ong Soo Soon                   *
      *                   Ref# R2-257                                       *
      *           Fixes on Partial Receipt Cancellation                     *
      *                                                                     *
      * 01/11/01  01/01   FA2148       Daryll Ng - CSC Malaysia             *
      *           RETROFIT FIXES FROM HONG KONG.                            *
      *           22/10/01  HR2030       Boboy                              *
      *           Entered valid Gl Account Code but message 'Account        *
      *           NOT on file' still displayed.                             *
      *                                                                     *
      * 01/11/01  01/01   FA2149       Daryll Ng - CSC Malaysia             *
      *           RETROFIT FIXES FROM HONG KONG.                            *
      *           22/10/01   HR2031       Boboy                             *
      *           When creating receipt for foreign currency, the           *
      *           message 'Orig Curr Do Not Balance' even if the            *
      *           amount entered actually are actually correct i.e.         *
      *           no imbalance                                              *
      *                                                                     *
      * 07/12/01  01/01   V62P15       Anggriani Susilo - CSC Indones       *
      *           When user presses <Enter> to complete a Receipt           *
      *           transaction, submit an AT job to print the Claim          *
      *           Facing Sheet.The AT subroutine will process all           *
      *           claims recovery transaction and check the Print           *
      *           F/S flag in the Claim Header. If the value is 'Y',        *
      *           it will proceed to print the Claim Facing Sheet.          *
      *                                                                     *
      * 20/12/01  01/01   V62P15       David Ong Soo Soon                   *
      *           If called from Business Object, then don't submit AT      *
      *           to print Claim Facing Sheet. If the Business Object is    *
      *           called via AT (ie. payment immediate post), the Business  *
      *           Object will crash.                                        *
      *                                                                     *
      * 18/01/02  01/01   PCPPRT       Saw Geok Tin                         *
      *           Change WSAA-RECEIPT-LETTER value 'RECEIPT'.               *
      *                                                                     *
      * 22/01/02  01/01                Saw Geok Tin                         *
      *           Retrofit AQR2344 & FA2303
      * 06/12/01  01/01   AQR2344      Wang Ge                              *
      *           When the T3719, 'Entity Must Be Blank' is Y, the  Entity  *
      *           is not required.                                          *
      *                                                                     *
      * 08/01/02  01/01   FA2303       Boboy                                *
      *           Problem: Cannot cancel receipt that has claims dissection *
      *           and if the receipt is not the last transaction entered    *
      *           for the particular claim. For example, receipt for salvage
      *           recovery is entered for a claim. Then another receipt was
      *           entered involving the same claim. System will not allow
      *           the cancellation of the 'first' receipt.
      *                                                                     *
      * 25/02/02  01/01   FA2442       Boboy                                *
      *           Original Amount wrongly updated with the Accounting       *
      *           Currency Amount in RBNK for Foreign Currency Receipt.     *
      *                                                                     *
      * 01/03/02  01/01   FA2363       Heng Cheng Kin                       *
      *           Move CKDS-TRANDESC only when it is found. If not found    *
      *           then keep the user input transaction description.         *
      *                                                                     *
      * 03/04/02  01/01   FA2479       Jane Kok Chiew Ping - CSC Mala       *
      *           Retrofit from Singapore.
      *           18/03/02   CSC127    LEE KIM PAU.
      *           Program display 'Fund/Terr.Code not match' when entering  *
      *           a GL/A with a gl account which is yet to be defined       *
      *                                                                     *
      * 17/04/02  01/01   FA2509       David Ong Soo Soon                   *
      *           Read from TR272 first before calling PRTCLMF AT routine.  *
      *           If item don't exist, then don't call.                     *
      *                                                                     *
      * 17/04/02  01/01   V63P28       David Ong Soo Soon                   *
      *           Call P2200UPKK only if the subroutine exists.             *
      *                                                                     *
      * 02/05/02  01/01   V63F29       Wishnu Priananto                     *
      *           Add checking for new sacscode (CV - Covernote)            *
      *
      * 11/12/02  01/01   FA1640       Chu Kok Kuan
      *           System still accepts if the amount in header
      *           is not tally to the marking records.
      *           To update subfile if there is changes in
      *           the subfile record.
      *                                                                     *
      * 19/12/02  01/01   V64F13       Lian Chui Tin                        *
      *           Update Debtor Marry Indicator entered in Receipt          *
      *           Header screen.                                            *
      *                                                                     *
      * 29/01/03  01/01   PCPRT2       DAN RIDON                            *
      *          -PC Printing retrofits.                                    *
      *                                                                     *
      * 17/03/03  01/01   FA2980       Nagamuthu S.Ramaiah                  *
      *           Include code RA/A for Fund Accounting checking.           *
      *                                                                     *
      * 10/04/03  01/01   FA2978       Nagamuthu S.Ramaiah                  *
      *           Recompile due to T3688REC copybook field rename           *
      *                                                                     *
      * 31/05/03  01/01   V65F04       Ong Saw Hoong - CSC Malaysia         *
      *           Include withholding tax                                   *
      *                                                                     *
      * 17/06/03  01/01   V65P23       Ali Hartono                          *
      *           Include VAT on sales (VS) and VAT on purchase (VP).       *
      *           It also prints VAT on sales document.                     *
      *                                                                     *
      * 25/07/03  01/01   V65F04       Ganesh Ramasubramanian               *
      *           Glcode is not updated for those sacscodes where the       *
      *           GLMAP is set as ALL STARS. These codes should not be      *
      *           calling GLSUBST. This is a short term solution as this    *
      *           program requires change whenever a new SACSCODE is        *
      *           introduced with GLMAP as ALL STARS.                       *
      *                                                                     *
      * 08/10/03  01/01   FA3062       Boboy                                *
      *           Skip display of warning message EFA0001 when called       *
      *           from BO.                                                  *
      *                                                                     *
      * 28/10/03  01/01   FA3131       Venkatesh Senthamarai.               *
      *           Linking application Number for company Suspense.          *
      *           If Sub account code is Company suspense there should not be*
      *           any restriction in the number of characters a user can put*
      *           in the description field. For normal cases description is *
      *           defaulted from the table if number of charactere is <= 8  *
      *                                                                     *
      * 06/05/04  01/01   FA3060       Nancie lin - FSG Taiwan              *
      *           To protect S2610-SELNUM for Full receipt Cancellation??   *
      *                                                                     *
      * 26/05/04  01/01   FA3331       Doris Yu                             *
      *           To comment out LETCUTISKM which is not used in program.   *
      *                                                                     *
      * 02/09/04  01/01   V66G24       Teddy Lim                            *
      *           Incorporate a new SACSCODE 'FL' for Float accounts        *
      *           used in GROUP/400.                                        *
      *                                                                     *
      * 09/09/04  01/01   FA3472       Tan Boo Kean                         *
      *           Standardlize the key when retrive data from               *
      *           RTVPRTF subroutine (TR272).                               *
      *                                                                     *
      * 16/09/04  01/01   FA3503       Simon Wong - CSC HK                  *
      *           Retrieve the document type after Function 13 is entered.  *
      *                                                                     *
      * 04/10/04  01/01   MLS006       Lian Chui Tin                        *
      *           Recompiled for promotion.                                 *
      *                                                                     *
      * 07/10/04  01/01   FA3384       Simon Wong - CSC HK                  *
      *           Like payment requisition, update sub ledger account       *
      *           with the cancel receipt number in SC type transaction.    *
      *                                                                     *
      * 09/11/04  01/01   SM0404       SMART/400 Release 0404 User          *
      *           Recompiled for promotion                                  *
      *                                                                     *
      * 24/02/05  01/01   FA3588       WORACHART                            *
      *           Extend size of sub-file variables in working storage to   *
      *           5 digits so that they could keep the sub-file record up   *
      *           to no 9999.                                               *
      *                                                                     *
      * 29/04/05  01/01   FA3600       Ani Susilo - CSC Indonesia           *
      *           Retrofit the following                                    *
      *           01/01   FA3600       Aristotle Kamal                      *
      *           AXA Reference No. R8-016S.                                *
      *           Variable WSAA-CLNTKEY should be re-initialized            *
      *           in 2000 section.  This is used to update RTRN             *
      *           record.  This value does not get changed when             *
      *           user press F13 - and change the client.                   *
      *                                                                     *
      * 18/05/05  01/01   V66P11       Ong Saw Hoong - CSC Malaysia         *
      *          -For Partial Cancellation, the field RCPTREV of RBNKPF     *
      *           file should be updated regardless which dissection of     *
      *           receipt header being selected.                            *
      *          -Added a new check box for Cover Note Details.             *
      *                                                                     *
      * 25/07/05  01/01   FA3701       Achara Chaipoom - CSC Thailand       *
      *           Reference no. TH/P1/R9/000046                             *
      * 18/07/05  01/01   R90319                                            *
      *           Missing record in RBNK file.                              *
      *                                                                     *
      * 22/06/06  01/01   FA4031       Fruto Araceli Ilustre - CSC Ph       *
      *           Retrofit from BPI/MS Fixes (David Ong 23/12/05)           *
      *           Foreign currency payment results in a 1 cent imbalance    *
      *           due to rounding errors. Allow a 1 cent tolerance and park *
      *           it into the first subfile record.                         *
      *                                                                     *
      * 23/06/06  01/01   FA4032       Fruto Araceli Ilustre - CSC Ph       *
      *           Retrofit from BPI/MS Fixes (Kok Kuan 05/05/06)            *
      *           Do not process dissections when S2610-INDIC = 'A' or '2'  *
      *           S2610-INDIC = 'A' or '2'.                                 *
      *                                                                     *
      * 12/07/06  01/01   WEBNQ1       Fruto Araceli Ilustre - CSC Ph       *
      *           Retrofit from WEBNQ1       (Lau Ka Kit 18/04/06)          *
      *           For remote program, update WSSP-SUBFILE-END as            *
      *           SCRN-SUBFILE-END.                                         *
      *           Retrofit from WEBNQ1       (Henry Wong 19/04/06)
      *           For remote program, don't populate the warning R012.      *
      *                                                                     *
      * 24/08/06  01/01   FA3968       Chu Kok Kuan - CSC Malaysia          *
      * R9-057M   To expand WSAA-SR289-SFL-END from 9(02) to 9(05).
      *                                                                     *
      * 01/09/06  01/01   V70G03       Chu Kok Kuan - CSC Malaysia          *
      *           Retrofit the following:                              ??   *
      * 29/08/06  01/01   V70G03       Tan Yoke Wah - CSC Singapore(RETROFIT
      * 26/04/06  01/01   V70G03       Durr Ariane Franceline Rose -
      *           Billing & Collection at member level.
      *                                                                     *
      * 08/11/06  01/01   V71F07       Durr Ariane Franceline Rose -        *
      *           Include F8, Bulk receipt                             ??   *
      *                                                                     *
      * 23/11/06  01/01   FA3943       Durr Ariane Franceline Rose -        *
      *           Retrofiting                                          ??   *
      * 12/04/06  01/01   FA3943       Ana Edy Poerwati - CSC Indones       *
      *           Reference QBE_037                                         *
      *           Batch not balance - due to partial receipt cancellation   *
      *                                                                     *
      *           For Partial Receipt Cancellation, validate that the       *
      *           selection number should be '2' since only if the          *
      *           selection number is '2' for partial receipt cancellation  *
      *           the dissection is updated in 3500- section.               *
      *                                                                     *
      * 21/04/06  01/01   FA3943       Susilo Angrianni - CSC Indones       *
      *           - For Partial Receipt Cancellation, accumulate the        *
      *             amount only for the selected ones                       *
      *                                                                     *
      *           Retrofit the following                                    *
      *           07/04/06  01/01   QBE041       Aristotle Kamal - CSC      *
      *           Fix imbalance amount in receipt.                          *
      *                                                                     *
      * 22/05/06  01/01   FA3943       Ida Yuna Leppina - CSC Indones       *
      *           Imbalance amount incorrect, after refresh (F5) and        *
      *           update/add/delete dissection amount.                      *
      *                                                                     *
      * 01/12/06  01/01   V71F07       Tan Yoke Wah - CSC Singapore         *
      *           Remove the checking of 1st subfile record must be entered *
      *           since F8 can be done without any record entered.          *
      *
      *           Due to definition of TRANSEQ in S2610 has been changed    *
      *           from 4S to 4A, need to initialise S2610-TRANSEQ to zeroes *
      *           when moving to CSHD-TRANSEQ.                              *
      *                                                                     *
      *           Fix the GROUP receipt by debtor number (retrofit)         *
      *                                                                     *
      * 02/05/07  01/01   V72P02       Ariane Durr/FSG/CSC (Singapore       *
      *           Cash before cover. Move WSSP-FSUCO to CSHD-FSUCO.    ??   *
      *           This is required by the editing routine for sub
      *           account code 'CN'
      *                                                                     *
      * 03/05/07  01/01   V72P02       Nath Mathmaluwe- Singapore           *
      *           Cash Before Cover: Check S2610-SACSTYPW = F, BG or CD     *
      *           and move S2610-SACSCODE to OPTS-SEL-CODE.                 *
      *                                                                     *
      * 11/05/07  01/01   PA4196       Ariane Durr/FSG/CSC (Singapore       *
      *           Disallow F8 when cancel(I) or Partial cancel(J)      ??   *
      *                                                                     *
      * 14/03/08  01/01   FA4263       Nath Mathmaluwe/FSG/CSC (Singa       *
      *           Change WSAA-SR22P-NEW-REC to 'N' when returning from      *
      *           any screen except from SR22P. This will avoid looping
      *           of pop-up screens when there is a F8-Bulk Receipts.
      *                                                                     *
      * 04/07/08  01/01   V73G32       Xu Chen/ASIA/CSC (Beijing)           *
      *           When payment type = '2'(cheque payment),write file CHQR,  *
      *           mark cheque realised flag with 'N'.                       *
      *                                                                     *
      * 15/07/08  01/01   V73G34       Siew Foong Liew/FSG/CSC (Beiji       *
      *           Updating of CHQR based on indicator setup in TR9QK.       *
      *                                                                     *
      * 20/10/08  01/01   V73G34       Ariane Durr/FSG/CSC (Singapore       *
      *           Recompile
      *                                                                     *
      * 20/10/08  01/01   GP4416       Vincent S Carillo/ASIA/CSC (Si       *
      *           Incorrect Payor NOT validated when F13 pressed ...        *
      *                                                                     *
      * 30/11/08  01/01   V73G49       Ramamurthi Sundar/FSG/CSC (Sin       *
      *           Recompile                                            ??   *
      *                                                                     *
      * 30/06/09  01/01   V74G02       Adelfa Otero Cabulao/ASIA/CSC        *
      *           Handle partial cancellation.                              *
      *                                                                     *
      * 30/06/09  01/01   GP5152       Adelfa Otero Cabulao/ASIA/CSC        *
      *           Validate duplicate dissection.                            *
      *                                                                     *
      * 27/07/09  01/01   FA4568       Nath Mathmaluwe/FSG/CSC (Singa       *
      *           Helpdesk #: 602393-XS09
      *           If a receipt is created for value  0.01,  system is       *
      *           allowing creation of receipt even if no input (SACS       *
      *           Code/Type, Dissection and amount) is provided in the      *
      *           screen.                                                   *
      *           Current system allows an imbalance of +0.01 or -0.01      *
      *           , but there should be atleast one dissection entry        *
      *           to continue with receipt creation.                        *
      *                                                                     *
      * 07/08/09  01/01   GP5152       Pinky Celestine B Cortez/ASIA/CSC (M *
      *           Pass appropriate function to handle F13 in journal        *
      *                                                                     *
      * 27/09/09  01/01   V74G02       Ramamurthi Sundar/FSG/CSC (Sin       *
      *           Include SACS code GM in addition to FL                    *
      *                                                                     *
      * 10/03/10  01/01   V75G04       Eubon Gicain/ASIA/CSC (Singapo       *
      *           Include SACS code GX                                      *
      *                                                                     *
      * 16/06/10  01/01   FA4882       Josefa Francisco De Jesus/SGP/       *
      *           Problem # 307379-XS10                                     *
      *           Dissection not save in receipt header (SR289) when        *
      *           press F13 to enter additional dissection.                 *
      *                                                                     *
      *           WSAA-SR289-SFL-END is not updated when returning          *
      *           from the secondary switching.  This variable is used      *
      *           to determine the total number of subfile record in        *
      *           SR289.                                                    *
      *                                                                     *
      *           Update WSAA-SR289-SFL-END when returning from SR289.      *
      *                                                                     *
      * 23/06/10  01/01   FA4888       Josefa Francisco De Jesus/SGP/       *
      *           Problem #894320-XS10                                      *
      *           Decimal data error encountered when try to edit the       *
      *           Receipt Header using F13 in Receipt Details screen        *
      *           (S2610) and program switched back.                        *
      *                                                                     *
      *           This happens when subfile records in Receipt Details      *
      *           screen (S2610) included  dissection(s) with SACSCODE      *
      *           that has program switching set up in T1661. e.g. CL,      *
      *           WH, VS, etc.                                              *
      *                                                                     *
      *           When passing thru 2000 section back from Receipt          *
      *           Header screen (PR289) to validate the entry against       *
      *           the changes made, by pass the 2670-CHCK-SELECTION as      *
      *           it set off OPTS-SEL-OPTNO to 1 and trigger the            *
      *           program in 4000 section to execute program switching      *
      *           which should not be the case since the OPTSWCH            *
      *           parameters is pointing to the wrong data and current      *
      *           logic for function key when they are performed is; it     *
      *           should not cause the subfile switching to be trigger.     *
      *                                                                     *
      * 18/07/10  01/01   FA4905       Nath Mathmaluwe/FSG/CSC (Singa       *
      *           Helpdesk#:231809-XS10                                     *
      *           Checking of dissection must do only when press ENTER.     *
      *           This is a further enhancement to FA4568.                  *
      *                                                                     *
      * 02/08/10  01/01   FA4915       Josefa Francisco De Jesus/SGP/       *
      *           The subfile validation is skip causing the system to      *
      *           encounter error/saved incorrect dissection.               *
      *           Happen when:                                              *
      *            1)Enter correct dissection and balance receipt amount    *
      *            2)Press F5 for the system to run the validation in       *
      *              2000 without exiting the screen.                       *
      *            3)Modify dissection to enter wrong Sacs Code; or         *
      *              Sacs Type; or Dissection Key.                          *
      *            4)Press enter.                                           *
      *            5)System captures the error.                             *
      *            6)Press enter again without modifying the subfile        *
      *              record(s)                                              *
      *           Result:                                                   *
      *            1)Incorrect dissection saved or ;                        *
      *            2)If enter incorrect SACSCODE, system will encounter     *
      *              error in 3000- section when updating the dissection    *
      *           Solution:                                                 *
      *           Perform 2400-DISSECTION-COUNT section only if             *
      *           WSSP-EDTERROR = O-K.  As in 2400-DISSECTION-COUNT         *
      *           section the subfile is read from start, causing the       *
      *           subfile last changed record to be reset.  In this         *
      *           case in the second time we pass SRNCH in the SCRN         *
      *           function system will encounter ENDP and                   *
      *           2600-VALIDATE-SUBFILE will not be perform.                *
      *                                                                     *
      * 24/08/10  01/01   V76F02       Josefa Francisco De Jesus/SGP/       *
      * 14/10/10  01/01   V76F02       Jessico Nocon/FSG/CSC (Singapo       *
      *           Introduce double confirmation before completing the       *
      *           receipt creation/cancelation. Base upon the setup         *
      *           in T3605.                                                 *
      *                                                                     *
      *           Add new field, USRNAME and USERID to display user         *
      *           information.                                              *
      *                                                                     *
      * 10/09/10  01/01   V76F06       Nancie Lin/FSG/CSC (Hong Kong)       *
      *           Call ZRDECPLC to do rounding processing                   *
      *                                                                     *
      * 16/09/10  01/01   V76F07       Nath Mathmaluwe/FSG/CSC (Singa       *
      * 18/11/10  01/01   V76F07       Josefa Francisco De Jesus/SGP/       *
      *           If T3605 Receipt Payor Defaulting is setup accordingly    *
      *           use Payor name as default dissection description.
      *           Solution:
      *           Move Client Key to CSHD-CLNTKEY before calling            *
      *           the validation subroutine and move Client Key to          *
      *           CKDS-CLNTKEY before calling the CKTRDSC subroutine.       *
      *                                                                     *
      * 04/10/10  01/01   V76F04       Nath Mathmaluwe/FSG/CSC (Singa       *
      * 16/11/10  01/01   V76F04       Josefa Francisco De Jesus/SGP/       *
      *           Recompile only.                                           *
      *                                                                     *
      * 11/10/10  01/01   FA4960       Nath Mathmaluwe/FSG/CSC (Singa       *
      *           Helpdesk #: 161597-XS10
      *           When there is a mix of accounting and foreign             *
      *           currency payment dissections and if there is an           *
      *           imbalance of 0.01 or -0.01 for ACCOUNTING currency,       *
      *           system adds the imbalance to the first dissection, if     *
      *           it is a foreign currency entry. System does this by       *
      *           assuming imbalance caused due to the exchange rate.       *
      *           Solution:                                                 *
      *           During the current imbalance calculation, do a            *
      *           separate imbalance calculation for accounting             *
      *           currency only entries. If the imbalance is caused by      *
      *           the accounting currency dissections, display a            *
      *           message to re-check the dissection amounts.               *
      *                                                                     *
      * 26/11/10  01/01   V76F12       Josefa Francisco De Jesus/SGP/       *
      *           Receipt Status Enhancement.                               *
      *           ===========================                               *
      *       (I) This enhancement has an option to turn off/on via         *
      *           table TR29S in FSU level.  If the switch is turned        *
      *           off, the Receipt should behave exactly the same way       *
      *           as in V7.5                                                *
      *                                                                     *
      *      (II) Introduce a receipt status that may track the many        *
      *           stages in its life cycle.  The receipt status is          *
      *           dictate by the setup in T3676, according to the           *
      *           transaction code.                                         *
      *           The setting up of receipt status is done in Receipt       *
      *           header screen SR289.                                      *
      *                                                                     *
      *     (III) Introduce Post-Dated Receipt.                             *
      *           Variable WSSP-CHEQ-PRESFLAG is used to hold the           *
      *           postdated flag and this variable is initialize in         *
      *           receipt header screen SR289.                              *
      *                                                                     *
      *           If Postdated, the reciept is saved in mirror file         *
      *           RCPTPF.                                                   *
      *                                                                     *
      *           Facilitate for the printing of postdated receipt.         *
      *                                                                     *
      *      (IV) Modify Receipt.                                           *
      *           'Regular' Receipt.                                        *
      *           The dissection screen is not editable as well as the      *
      *           windowing screen. The validation is same as in            *
      *           inquiry mode as the user may make selections.             *
      *                                                                     *
      *           Postdated Receipt.                                        *
      *           If printed, the validation is same as 'Regular'           *
      *           Receipt otherwise;                                        *
      *           all fields and windowing screen can be edited and         *
      *           should behave as Create Receipt.                          *
      *           In the event that a postdated receipt is converted        *
      *           to 'regular' receipt, upon saving it will delete          *
      *           the record in the mirror file and post it in RTRN.        *
      *                                                                     *
      * 16/12/10  01/01   V76F13       Josefa Francisco De Jesus/SGP/       *
      *           Credit card details.                                      *
      *           - Cater for the saving of new fields for Credit Card      *
      *             details.                                                *
      *           - writes GL Account Key in RTRN(Header Record) to         *
      *             read T3688 Credit Card GL account if  Payment type      *
      *             in Receipt screen = '9' (Credit card)                   *
      *                                                                     *
      * 13/01/11  01/01   V76F06       Wang Ge/FSG/CSC (Singapore)          *
      *           - Rounding also apply to S2610-ORIGAMT, to ensure it is   *
      *             tally with the rounding rule                            *
      *           - For rounding balance checking, change to base on the    *
      *             currency rounding rule that set up on 0T1667 to         *
      *             determine imbalance tolerance amount:
      *             1. allow 0.01 for 2 decimal rounding                    *
      *             2. allow 0.10 for 1 decimal rounding                    *
      *             3. allow 1.00 for 1 integer rounding                    *
      *             4. allow 10.00 for 2 integer rounding                   *
      *             so on...                                                *
      *                                                                     *
      * 08/04/11  01/01   FA5241       Josefa Francisco De Jesus/SGP/       *
      *           Help Desk #484847-XS11                                    *
      *           Unbalanced Currency Transactions causing to post          *
      *           imbalance GL entries accounts in GTRNPF.                  *
      *                                                                     *
      *           Consider this Receipt transaction:                        *
      *           ----------------------------------                        *
      *           Doc Amt: 1500 SGD (Bank)                                  *
      *           Dissections Amt:                                          *
      *           GL Amount                                                 *
      *            ACCT AMT                     ORIG AMT    RATE            *
      *            ----------                   ---------   --------        *
      *            2250.00SGD accounted for as  1500.00USD  1.500000        *
      *           -2100.00SGD accounted for as -1500.00USD  1.400000        *
      *            1350.00SSG accounted for as  1350.00SGD  1.000000        *
      *                                                                     *
      *           This transaction would appear to be balance as the        *
      *           accounting currency amounts balance to zero againts       *
      *           our document amount.  Thus it will give us GL balance     *
      *           error:                                                    *
      *                 SGD                  USD                            *
      *           AcctAmt|OrigAmt      AcctAmt|OrigAmt                      *
      *           -------------------------------------                     *
      *             150  |  150          150- |   0                         *
      *                                                                     *
      *           The rule is complied with by posting only the             *
      *           accounting amount when the currencies are the same        *
      *           but posting both the original and accounting              *
      *           currencies when they are different.                       *
      *                                                                     *
      *           Solution:                                                 *
      *           Ensure that the total original amount per currency        *
      *           must be equal to 0.  This validation is base on the       *
      *           GL Post imbalance error rules.                            *
      *                                                                     *
      * 29/07/11  01/01   FA5281       Josefa Fransisco De Jesua FSG/       *
      *           Further Fixes of FA5241.                                  *
      *           - Checking of GL imbalance amount should take place       *
      *             after all the validation and after the function key     *
      *             is invoke.  However, since F7-Print is similar to       *
      *             receipt confirmation it is excluded as all              *
      *             validation must be satisfied.                           *
      *                                                                     *
      * 16/05/12  01/01   GP5380       E Ryan R Alilin/ASIA/CSC (Sing       *
      *           Retrofit from GE - ZGP036.                                *
      *           Initialise CSHD-CHDRNUM.                                  *
      *                                                                     *
      * 25/05/12  01/01   FA5381       Josefa Fransisco De Jesua FSG/       *
      *           Fix bug on partial receipt cancellation, where it         *
      *           return error RFKE - Acct Entries Not Balance because      *
      *           the subfile record marked as "2-Cancel" is not            *
      *           considers.                                                *
      *                                                                     *
      * 02/07/12  01/01   FA5389       Josefa Fransisco De Jesua FSG/       *
      *           HelpDesk Issue#520593-XS12                                *
      *           The receipt save duplicate records when user input        *
      *           option "2" in the select fields in the subfile line       *
      *           and the action is not cancellation.                       *
      *                                                                     *
      *           Fix to return invalid selection when option is not        *
      *           applicable to the dissection type.                        *
      *                                                                     *
      * 06/08/13  01/01   PDEV01       SMART Release 1204 User              *
      *           Recompile due to T1667 copybook changes                   *
      *                                                                     *
      * 08/10/13  01/01   RC002        Khang Nguyen - CSC Developer         *
      *           Enhance system to apply Pre-printed Receipt               *
      *           TRANSACTION DATE: Get the Deposit Date instead of         *
      *                             Receipt Date (Transaction Date)         *
      *           TRANSACTION DESC: Get the description of sacs code/       *
      *                             sacs type instead of the name of        *
      *                             Policy Owner                            *
      *           Use WSSP-SERVUNIT to transfer Pre-Printed Code            *
      *                                                                     *
      * 21/10/13  01/01   GAPPH1       Tuan Le                              *
      *           Remove function F7 Print Online                           *
      *                                                                     *
      * 29/10/13  01/01   RC002        Sang Nguyen - CSC Developer          *
      *           RECOMP                                                    *
      *
      * 11/02/14  01/01   PHL109       Israez Prieto - CSC Developer        *
      *           Set Fund code with Contract type if the value is spaces.  *
      *                                                                     *
      * 12/03/14  01/01   PHE001       Thanh Do                             *
      *           Recompile.                                                *
      *                                                                     *
      * 13/03/14  01/01   PHLRMS       Phuong Le Dev                        *
      *           Update TRRN file                                          *
      *                                                                     *
      * 16/01/15  01/01   PHE003       Phuong Le Dev                        *
      *           Fix : - Skip error if not found in ZPPR                   *
      *                 - Only insert RCOL if Payment Type = '1'(Cash)      *
      *                                                                     *
      * 15/09/17  01/01   CS002        Tuyet Huynh IT - DEV                 *
      *           Update TRRN file.                                         *
      *                                                                     *
      * 25/01/18  01/01   PS008        Tuyet Huynh IT - DEV                 *
      *           Update 'Used-Bank' status when banked cash.               *
      *                                                                     *
      * 17/04/18  01/01   PHE010       Vo Thi Hong Duyen - IT               *
      *           Add validate not allow sac code LN.                       *
      *                                                                     *
      * 15/06/18  01/01   CS009        Tuyet Huynh IT - DEV                 *
      *           Default Sacs Code: LP , Type: S                           *
      *           when link from RMS information to Receipt.
      *                                                                     *
      * 27/02/19  01/01   CS016        Tuyet Huynh IT - DEV                 *
      *           Add action M: only modify Payor and Application No.       *
      *                                                                     *
      * 08/11/19  01/01   PHFX54       Thanh Do                             *
      *           Correct EFFDATE for Action M of modified receipt.         *
      *                                                                     *
      * 23/09/20  01/01   CS023        Mai Yen Phi - IT                     *
      *           Recompile                                                 *
      *                                                                     *
      * 21/10/20  01/01   PS070        Mai Yen Phi - IT                     *
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
       01  WSAA-PROG                   PIC X(05) VALUE 'P2610'.
       01  WSAA-VERSION                PIC X(02) VALUE '02'.
      *
       01  WSAA-SUB-ACC.                                                <PHLRMS>
           03  WSAA-ARR-SUB-ACC        OCCURS 15.                       <PHLRMS>
               05  WSAA-ARR-SACSCODE   PIC X(02).                       <PHLRMS>
               05  WSAA-ARR-SACSTYPW   PIC X(02).                       <PHLRMS>
                                                                        <PHLRMS>
       01  WSAA-FLAG-UPDATE            PIC X(01).                       <PHLRMS>
       01  WSAA-COUNT-ERR-SUB-ACC      PIC 9(02).                       <PHLRMS>
       01  WSAA-COUNT-ERR-POLICY       PIC 9(02).                       <PHLRMS>
       01  WSAA-WARN                   PIC X(20)                        <PHLRMS>
           VALUE 'Confirm to continue:'.                                <PHLRMS>
       01  WSAA-SC                     PIC X(02) VALUE 'SC'.            <PHLRMS>
       01  WSAA-I                      PIC X(02) VALUE 'I '.            <PHLRMS>
       01  WSAA-VALID-SUB-ACC          PIC X(01).                       <PHLRMS>
       01  IDX                         PIC 9(02).                       <PHLRMS>
       01  WSAA-TRANNO                 PIC 9(05).                       <PHLRMS>
       01  WSAA-RCPT-TYPE              PIC X(02).                       <PHLRMS>
       01  WSAA-RCPT-NUM               PIC S9(08).                      <PHLRMS>
       01  WSAA-TABLE                  PIC X(05).                       <PHLRMS>
       01  WSAA-ITEM                   PIC X(08).                       <PHLRMS>
       01  WSAA-FILE-RRN               PIC 9(09) COMP-4.
       01  WSAA-TEXT                   PIC X(11).
       01  WSAA-DOCTDESC               PIC X(30).
       01  WSAA-TRANDESC-LEN           PIC S9(02) COMP-3 VALUE 30.
       01  WSAA-BANK-FUND              PIC X(03).
       01  WSAA-TODAY                  PIC 9(08).
       01  WSAA-TIME                   PIC 9(06).
       01  WSAA-COUNT                  PIC 9(02).
       01  WSAA-IX                     PIC 9(02).
       01  WSAA-TRANSEQ                PIC 9(04).
       01  WSAA-JRNSEQ                 PIC 9(04).
       01  WSAA-ONE-DISS.
           03  WSAA-RRN                PIC X(01) OCCURS 999.
       01  WSAA-MAX-OPT                PIC 9(02) VALUE 10.              <V72P02>
       01  WSAA-MAX-OPTA               PIC 9(02) VALUE 11.              <V72P02>
       01  WSAA-SCRN-STATUZ            PIC X(04).
       01  WSAA-SCRN-SCRNAME           PIC X(05).
       01  WSAA-SFL-CNT                PIC 9(02).
      *01  WSAA-SFL-RRN                PIC 9(02).                       <FA3588>
      *01  WSAA-SFL-END                PIC 9(02).                       <FA3588>
       01  WSAA-SFL-RRN                PIC 9(05).                       <FA3588>
       01  WSAA-SFL-END                PIC 9(05).                       <FA3588>
   ****01  WSAA-SR289-SFL-END          PIC 9(02).                       <FA3968>
       01  WSAA-SR289-SFL-END          PIC 9(05).                       <FA3968>
       01  WSAA-SR22P-NEW-REC          PIC X(01).                       <V71F07>
       01  WSAA-SR289-INDIC            PIC X(01).                       <GP4416>
   ****01  WSAA-IDENTIFIER             PIC X(10) VALUE 'RECEIPT'.       <FA3472>
      *                                                                 <FA3472>
       01  WSAA-RTVPRTF-ITEMKEY.                                        <FA3472>
           03  WSAA-RTVPRTF-LANGUAGE   PIC X(01).                       <FA3472>
           03  WSAA-RTVPRTF-IDENTIFIER PIC X(02) VALUE SPACES.          <FA3472>
           03  WSAA-RTVPRTF-ALL        PIC X(03) VALUE '***'.           <FA3472>
      *                                                                 <FA3472>
       01  WSAA-CL-STAR                PIC X(08) VALUE 'CL***   '.      <PS2509>
       01  WSAA-P2200UPKK              PIC X(10) VALUE 'P2200UPKK'.     <V63P28>
       01  WSAA-OBJTYPE                PIC X(08) VALUE '*PGM'.          <V63P28>
       01  WSAA-P2200UPKK-CALL         PIC X(01) VALUE SPACES.          <V63P28>
      *
       01  WSAA-SUBFILE-RRN.                                            <V62P04>
           03  WSAA-SFL-RRN01          PIC X(01).                       <V62P04>
           03  WSAA-SFL-RRN02          PIC X(04).                       <V62P04>
       01  WSAA-KFK-RRN                PIC 9(04).                       <V62P04>
       01  WSAA-CURRENT-RRN            PIC S9(05) VALUE ZEROS.          <R2-296>
       01  IX                          PIC S9(05) VALUE ZEROS.          <R2-296>
       01  IY                          PIC S9(05) VALUE ZEROS.          <R2-296>
      *
       01  WSAA-SUBSCRIPT              PIC S9(04) COMP-3.
       01  WSAA-DIFF                   PIC S9(15)V9(02) COMP-3.
       01  WSAA-DISS-TOTAL             PIC S9(15)V9(02) COMP-3.
       01  WSAA-DISS-COUNT             PIC 9(03)  COMP-3.
       01  WSAA-STORE-DOCORIGAMT       PIC S9(15)V9(2).
      *                                                                 <R2-296>
       01  WSAA-NUM-SELECTED           PIC S9(03)  COMP-3.              <R2-296>
       01  WSAA-SCRN-FUNCTION          PIC X(05) VALUE SPACES.          <R2-296>
       01  WSAA-FIRST-TIME             PIC X(01) VALUE SPACES.          <R2-296>
       01  WSBB-SACSCODE               PIC X(02).                       <FA3131>
           88 COMPANY-SUSPENSE         VALUE 'SC'.                      <FA3131>
      *
       01  WSAA-NO-MORE-BLANC-LINE     PIC X(01).                       <V71F07>
           88 NO-MORE-BLANC-LINE       VALUE 'Y'.                       <V71F07>
      *                                                                 <V71F07>
       01  WSAA-NEW-RECEIPT            PIC X(09).
       01  WSAA-OLD-RECEIPT            PIC X(09).
      *                                                                 <V64F13>
       01  WSAA-MARRYFLAG              PIC X(01).                       <V64F13>
       01  WSAA-PRINT-WH-CERT          PIC X(01).                       <V65F04>
       01  WSAA-DUPL-WH-SACSCODE       PIC X(01).                       <V65F04>
       01  WSAA-WH-SUBFILE-RRN         PIC 9(05) COMP-3.                <V65F04>
      *01  WSAA-WHTAXCRT       PIC X(08) VALUE 'WHTAXCRT'.      <FA3472><V65F04>
                                                                        <V65F04>
       01  WSAA-TRANS-AREA.                                             <V65F04>
           03  WSAA-TRANS-FSUCO        PIC X(01).                       <V65F04>
           03  FILLER                  PIC X(199).                      <V65F04>

      * PC printing variables
       01  WSAA-FUNCTION               PIC X(05) VALUE 'START'.
       01  WSAA-PCP-MODE               PIC X(01).
       01  WSAA-LETC-RRN               PIC S9(09) COMP-3.
       01  WSAA-FORMAT-FOR             PIC X(10).
       01  WSAA-STATUZ                 PIC X(04) VALUE '****'.
      *01  WSAA-RECEIPT-LETTER         PIC X(10) VALUE 'ZRECEIPT'.      <PCPPRT>
       01  WSAA-RECEIPT-LETTER         PIC X(10) VALUE 'RECEIPT'.       <PCPPRT>
       01  WSAA-RECEIPT-LETTERP        PIC X(10) VALUE 'RECEIPTP'.      <V76F12>
       01  WSAA-VAT-ON-SALES           PIC X(10) VALUE 'P2610UPVS'.     <V65P23>
       01  WSAA-VATS-CALL              PIC X(01) VALUE SPACES.
      *
       01  WSAA-CANL-BATCKEY.
           03  WSAA-CANL-BATCPFX               PIC X(0002).
           03  WSAA-CANL-BATCCOY               PIC X(0001).
           03  WSAA-CANL-BATCBRN               PIC X(0002).
           03  WSAA-CANL-BATCACTYR             PIC S9(04) COMP-3.
           03  WSAA-CANL-BATCACTMN             PIC S9(02) COMP-3.
           03  WSAA-CANL-BATCTRCDE             PIC X(0004).
           03  WSAA-CANL-BATCBATCH             PIC X(0005).
      *
       01  WSAA-SAVE-BATCKEY.
           03  WSAA-SAVE-BATCPFX               PIC X(0002).
           03  WSAA-SAVE-BATCCOY               PIC X(0001).
           03  WSAA-SAVE-BATCBRN               PIC X(0002).
           03  WSAA-SAVE-BATCACTYR             PIC S9(04) COMP-3.
           03  WSAA-SAVE-BATCACTMN             PIC S9(02) COMP-3.
           03  WSAA-SAVE-BATCTRCDE             PIC X(0004).
           03  WSAA-SAVE-BATCBATCH             PIC X(0005).
      *
       01  WSAA-ACC-CODE.
           03   WSAA-SACSCODE          PIC X(02)  VALUE SPACES.
           03   FILLER                 PIC X(01)  VALUE '/'.
           03   WSAA-SACSTYPW          PIC X(02)  VALUE SPACES.

       01  WSAA-TR384-ITEMITEM.                                         <PCPRT2>
           03  FILLER                  PIC X(03)  VALUE '***'.          <PCPRT2>
           03  WSAA-TRANSCODE          PIC X(04).                       <PCPRT2>
                                                                        <PCPRT2>
       01  WSAA-EXTRA-KEY.
           03  WSAA-PTCODE             PIC X(02).
           03  WSAA-PAYESEL            PIC X(08).
      *
       01  WSAA-DOCTKEY.
           03  WSAA-RECEIPT-PFX        PIC X(02).
           03  WSAA-RECEIPT-COY        PIC X(01).
           03  WSAA-RECEIPT            PIC X(09).

       01  WSAA-TRANKEY.
           03  WSAA-TRANPFX            PIC X(02).
           03  WSAA-TRANCOY            PIC X(01).
           03  WSAA-ENTITY             PIC X(16).
      *
       01  WSAA-WTRANKEY.                                               <FA3384>
           03  WSAA-WTRANPFX           PIC X(02).                       <FA3384>
           03  WSAA-WTRANCOY           PIC X(01).                       <FA3384>
           03  WSAA-WENTITY            PIC X(16).                       <FA3384>
      *                                                                 <FA3384>
       01  WSAA-ENTITY-123.
           03  WSAA-ENTITY-1           PIC X(16).
           03  WSAA-ENTITY-2           PIC X(16).
           03  WSAA-ENTITY-3           PIC X(16).
                                                                        <V6F114>
       01  WSAA-PAXMSG.                                                 <V6F114>
           03  WSAA-LANGUAGE           PIC X(01).                       <V6F114>
           03  WSAA-SERVICE-UNIT       PIC X(02) VALUE 'FA'.            <V6F114>
           03  WSAA-MSGID              PIC X(04).                       <V6F114>
       01  WSAA-INTER-BRANCH-FLAG      PIC X(01).                       <V6F114>
           88 INTER-BRANCH-YES         VALUE 'Y'.                       <V6F114>
           88 INTER-BRANCH-NO          VALUE 'N'.                       <V6F114>
      *
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
      *
       01  WSAA-POPUP-SACSCODE         PIC X(02).
           88  MANDATORY-POPUP         VALUE 'CL'.
      *
       01  WSAA-ACC-ACCODE             PIC X(05).
           88   WSAA-VALID-ACCODE      VALUE 'FG/PP' 'CL/CS'
                                             'AG/A ' 'AG/PC'
                                             'GL/A ' 'RI/A'
                                             'RA/A '                    <FA2980>
                                             'TP/A ' 'TB/A'.
      *
      *01  WSAA-SWITCH-ACTIVE          PIC X(01) VALUE 'N'.             <R2-296>
      ***  88   SWITCH-ACTIVE          VALUE 'Y'.                       <R2-296>
      *
      *01  WSAA-ONE-SELECTED           PIC X(01) VALUE 'N'.             <R2-296>
      ***  88   ONE-SELECTED           VALUE 'Y'.                       <R2-296>
      *
       01  WSAA-FOUND-SELECTION        PIC X(01) VALUE 'N'.
           88   FOUND-SELECTION        VALUE 'Y'.
      *
       01  WSAA-FOUND-FKEY             PIC X(01) VALUE 'N'.
           88   FOUND-FKEY             VALUE 'Y'.
      *
       01  WSAA-DISPLAY-IMBAL          PIC X(01) VALUE 'N'.
           88   DISPLAY-IMBAL                    VALUE 'Y'.
      *
       01  WSAA-PCPRT-ENABLE           PIC X(01) VALUE 'N'.
           88   PCPRT-ENABLE                     VALUE 'Y'.
      *                                                                 <R2-292>
       01  WSAA-FIRST-WARNING          PIC X(01) VALUE 'N'.             <R2-292>
           88   FIRST-WARNING                    VALUE 'Y'.             <R2-292>
                                                                        <V66P11>
       01  WSAA-SACSCODE-FG            PIC  X(02) VALUE 'FG'.           <V66P11>
       01  WSAA-SACSCODE-AG            PIC  X(02) VALUE 'AG'.           <V66P11>
       01  WSAA-SACSTYPE-PP            PIC  X(02) VALUE 'PP'.           <V66P11>
       01  WSAA-SACSTYPE-A             PIC  X(02) VALUE 'A'.            <V66P11>
                                                                        <CS016>
       01  WSAA-CODE                   PIC X(02).                       <CS016>
       01  WSAA-TYPE                   PIC X(02).                       <CS016>
       01  WSAA-TRANCODE               PIC X(04).                       <CS016>
       01  WSAA-TRANCODE-CUR           PIC X(04).                       <CS016>
       01  WSAA-UPDATE-FLAG            PIC X(01).
                                                                        <V66P11>
       01  WSAA-FIRST-READ             PIC  X(01).                      <V66P11>
           88  FIRST-READ              VALUE 'Y'.                       <V66P11>
                                                                        <V66P11>
       01  WSAA-FOUND-CHK-BOX          PIC  X(01).                      <V66P11>
           88  FOUND-CHK-BOX           VALUE 'Y'.                       <V66P11>
                                                                        <V66P11>
       01  WSAA-REVISIT-CHK-BOX        PIC  X(01).                      <V66P11>
           88  REVISIT-CHK-BOX         VALUE 'Y'.                       <V66P11>
                                                                        <V66P11>
       01  WSAA-ENTER-CN-SCREEN        PIC  X(01).                      <V66P11>
           88  ENTER-CN-SCREEN         VALUE 'Y'.                       <V66P11>
                                                                        <V66P11>
       01  WSAA-CN-CANCELLATION        PIC  X(01).                      <V66P11>
           88  CN-CANCELLATION         VALUE 'Y'.                       <V66P11>
                                                                        <V66P11>
       01  WSAA-COVER-NOTE-ALLOWED     PIC  X(01).                      <V66P11>
           88  COVER-NOTE-ALLOWED      VALUE 'Y'.                       <V66P11>
      *                                                                 <V76F12>
       01  WSAA-POSTDTEFLG             PIC X(01) VALUE ' '.             <V76F12>
           88 NOT-POSTDATED            VALUE ' '.                       <V76F12>
           88 P-POSTDATED              VALUE 'P'.                       <V76F12>
           88 Y-POSTDATED              VALUE 'Y'.                       <V76F12>
      *                                                                 <V76F12>
       01  WSAA-RCTSTATFLG             PIC X(01) VALUE ' '.             <V76F12>
           88 RCTSTATFLG               VALUE 'Y'.                       <V76F12>
      *                                                                 <V76F12>
       01  WSAA-PRINTED                PIC X(01) VALUE ' '.             <V76F12>
           88 PRINTED                  VALUE 'Y'.                       <V76F12>
      *                                                                 <V76F12>
       01  WSAA-SEQNBR                 PIC 9(03) VALUE ZEROES.          <V76F12>
       01  WSAA-NUMBER                 PIC 9(05) VALUE ZEROES.          <V76F12>
       01  WSAA-RBNK-POSTDTEFLG        PIC X(01) VALUE SPACES.          <V76F12>
      *
       01  WSAA-DP-SUBFILE-RRN         PIC 9(05) COMP-3.                <GP5152>
       01  WSAA-LAST-SUBFILE-RRN       PIC 9(05) COMP-3.                <GP5152>
       01  WSAA-FOUND-DUP              PIC X(01).                       <GP5152>
       01  WSAA-IN-LOOP-STATUZ         PIC X(04).                       <GP5152>
       01  WSAA-OUT-LOOP-STATUZ        PIC X(04).                       <GP5152>
       01  WSAA-DP-KEY.                                                 <GP5152>
           03   WSAA-DP-SACSCODE       PIC X(02)  VALUE SPACES.         <GP5152>
           03   WSAA-DP-SACSTYPW       PIC X(02)  VALUE SPACES.         <GP5152>
           03   WSAA-DP-ENTITY         PIC X(16).                       <GP5152>
       01  WSAA-KEY.                                                    <GP5152>
           03   WSAA-KEY-SACSCODE      PIC X(02)  VALUE SPACES.         <GP5152>
           03   WSAA-KEY-SACSTYPW      PIC X(02)  VALUE SPACES.         <GP5152>
           03   WSAA-KEY-ENTITY        PIC X(16).                       <GP5152>
       01  WSAA-DISSEC-COUNT           PIC 9(04).                       <FA4568>
       01  WSAA-NEW-BAL                PIC S9(15)V9(02) COMP-3 VALUE 0. <V74G02>
       01  WSAA-GLAMT-TOTAL            PIC S9(15)V99 COMP-3.            <FA4960>
       01  WSAA-GLIMBAL                PIC S9(15)V99 COMP-3.            <FA4960>
      *                                                                 <V76F02>
       01  WSAA-MBOX-REPLY.                                             <V76F02>
           03 WSAA-MBOX-DFT-ND         PIC X(01) VALUE 'X'.             <V76F02>
           03 WSAA-MBOX-DFT-FLAG       PIC X(03) VALUE 'DFT'.           <V76F02>
           03 WSAA-MBOX-DFT-VALUE      PIC X(61).                       <V76F02>
      *                                                                 <V76F06>
       01  WSAA-IMBAL                  PIC S9(09)V9(02).                <V76F06>
       01  WSAA-IMBAL-N                PIC S9(09)V9(02).                <V76F06>
      *                                                                 <FA5241>
       01  WSAA-MAX-GL-ENT             PIC 9(03) VALUE 99.              <FA5241>
       01  WSAA-GL-ENTRIES.                                             <FA5241>
           03  WSAA-GL-ENT             OCCURS 99.                       <FA5241>
               05  WSAA-GL-BCURR       PIC X(03).                       <FA5241>
               05  WSAA-GL-ORIGAMT     PIC S9(15)V9(2).                 <FA5241>
      *                                                                 <GP5152>
       01  WSAA-TALLY                  PIC 9(02) VALUE ZERO.            <PHL109>
                                                                        <PHLRMS>
       01  WSAA-TOPFX                  PIC X(02).                       <PHLRMS>
       01  WSAA-FRMPFX                 PIC X(02).                       <PHLRMS>
       01  WSAA-FROMPER                PIC X(08).                       <PHLRMS>
       01  WSAA-TOLOC                  PIC X(08).                       <PHLRMS>
      *                                                                 <PHLRMS>
       01  ERRORS.
           03  E141                    PIC X(04) VALUE 'E141'.
           03  E186                    PIC X(04) VALUE 'E186'.
           03  E192                    PIC X(04) VALUE 'E192'.
           03  E193                    PIC X(04) VALUE 'E193'.
           03  E198                    PIC X(04) VALUE 'E198'.
           03  E207                    PIC X(04) VALUE 'E207'.
           03  E289                    PIC X(04) VALUE 'E289'.
           03  E436                    PIC X(04) VALUE 'E436'.
           03  F042                    PIC X(04) VALUE 'F042'.
           03  F043                    PIC X(04) VALUE 'F043'.
           03  F044                    PIC X(04) VALUE 'F044'.
           03  F955                    PIC X(04) VALUE 'F955'.
           03  F946                    PIC X(04) VALUE 'F946'.
           03  F949                    PIC X(04) VALUE 'F949'.
           03  F962                    PIC X(04) VALUE 'F962'.
           03  H166                    PIC X(04) VALUE 'H166'.
           03  H457                    PIC X(04) VALUE 'H457'.          <FA2363>
           03  H933                    PIC X(04) VALUE 'H933'.
           03  G317                    PIC X(04) VALUE 'G317'.
           03  O001                    PIC X(04) VALUE 'O001'.
           03  R012                    PIC X(04) VALUE 'R012'.
      ***  03  RF25                    PIC X(04) VALUE 'RF25'.          <R2-296>
           03  RPAG                    PIC X(04) VALUE 'RPAG'.
           03  RF64                    PIC X(04) VALUE 'RF64'.          <V65F04>
           03  F612                    PIC X(04) VALUE 'F612'.          <V66P11>
           03  G602                    PIC X(04) VALUE 'G602'.          <PA4196>
           03  H357                    PIC X(04) VALUE 'H357'.          <GP5152>
           03  F413                    PIC X(04) VALUE 'F413'.          <FA4568>
           03  E778                    PIC X(04) VALUE 'E778'.          <V74G02>
           03  F465                    PIC X(04) VALUE 'F465'.          <FA4960>
           03  RFIK                    PIC X(04) VALUE 'RFIK'.          <V76F06>
           03  RFIL                    PIC X(04) VALUE 'RFIL'.          <V76F06>
           03  RFKE                    PIC X(04) VALUE 'RFKE'.          <FA5241>
           03  EV48                    PIC X(04) VALUE 'EV48'.          <PHLRMS>
           03  EV52                    PIC X(04) VALUE 'EV52'.          <PHLRMS>
      *
       01  TABLES.
           03  T1688                   PIC X(05) VALUE 'T1688'.
           03  T2634                   PIC X(05) VALUE 'T2634'.
           03  T3629                   PIC X(05) VALUE 'T3629'.
           03  T3688                   PIC X(05) VALUE 'T3688'.
           03  T3697                   PIC X(05) VALUE 'T3697'.
           03  T3676                   PIC X(05) VALUE 'T3676'.
           03  T3711                   PIC X(05) VALUE 'T3711'.
           03  T3719                   PIC X(05) VALUE 'T3719'.
           03  TR384                   PIC X(05) VALUE 'TR384'.         <PCPRT2>
           03  TR386                   PIC X(05) VALUE 'TR386'.
           03  TR272                   PIC X(05) VALUE 'TR272'.         <PS2509>
           03  TR29S                   PIC X(05) VALUE 'TR29S'.         <V76F12>
           03  TR9QK                   PIC X(05) VALUE 'TR9QK'.         <V73G34>
           03  T3605                   PIC X(05) VALUE 'T3605'.         <V76F02>
           03  T1667                   PIC X(05) VALUE 'T1667'.         <V76F06>
           03  T3695                   PIC X(05) VALUE 'T3695'.
           03  TV035                   PIC X(05) VALUE 'TV035'.         <PHLRMS>
           03  TV041                   PIC X(05) VALUE 'TV041'.         <PHLRMS>
           03  TV023                   PIC X(05) VALUE 'TV023'.         <PHLRMS>
      *
       01  FORMATS.
           03  BBALREC                 PIC X(10) VALUE 'BBALREC'.
           03  BATCREC                 PIC X(10) VALUE 'BATCREC'.       <V76F02>
           03  CHQRREC                 PIC X(10) VALUE 'CHQRREC'.       <V73G32>
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  GENLREC                 PIC X(10) VALUE 'GENLREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  RBNKREC                 PIC X(10) VALUE 'RBNKREC'.
           03  RDOCREC                 PIC X(10) VALUE 'RDOCREC'.
           03  RTRNREC                 PIC X(10) VALUE 'RTRNREC'.
           03  RCPTREC                 PIC X(10) VALUE 'RCPTREC'.       <V76F12>
           03  SANCREC                 PIC X(10) VALUE 'SANCREC'.       <V76F02>
           03  RBNKSEQREC              PIC X(10) VALUE 'RBNKSEQREC'.    <V76F12>
           03  ZPPRREC                 PIC X(10) VALUE 'ZPPRREC'.       <RC002>
           03  ZPPRENQREC              PIC X(10) VALUE 'ZPPRENQREC'.    <RC002>
           03  CHDRLIFREC              PIC X(10) VALUE 'CHDRLIFREC'.    <PHL109>
           03  TRRNREC                 PIC X(10) VALUE 'TRRNREC'.       <PHLRMS>
           03  TRRNTRNREC              PIC X(10) VALUE 'TRRNTRNREC'.    <PHLRMS>
           03  RCPYACTREC              PIC X(10) VALUE 'RCPYACTREC'.    <PHLRMS>
           03  RCOLPNDREC              PIC X(10) VALUE 'RCOLPNDREC'.    <PHLRMS>
           03  TTRCENQREC              PIC X(10) VALUE 'TTRCENQREC'.    <PHLRMS>
           03  CLRRENQREC              PIC X(10) VALUE 'CLRRENQREC'.    <CS016>
           03  RTRNDOCREC              PIC X(10) VALUE 'RTRNDOCREC'.    <CS016>
      *
       01  WSAA-POLNO                  PIC X(08).                       <PHLRMS>
                                                                        <PHLRMS>
       01  WSAA-BATCKEY.
           COPY BATCKEY.
      *
       01  WSAA-CLNTKEY.
           COPY CLNTKEY.
      *
       01  WSAA-AGNTKEY.
           COPY AGNTKEY.
      *
       01  WSAA-GENLKEY.
           COPY GENLKEY.
      *
       01  WSKY-ITEMKEY.
           COPY ITEMKEY.
      *
       01  WSAA-RBNKKEY.
           COPY RBNKKEY.
      *                                                                 <V76F02>
       01  WSAA-SANCKEY.                                                <V76F02>
           COPY SANCKYR.                                                <V76F02>
      /
           COPY T2634REC.
           COPY T3629REC.
           COPY T3676REC.
           COPY T3688REC.
           COPY T3697REC.
           COPY T3711REC.
           COPY T3719REC.
           COPY TR386REC.
           COPY TR29SREC.                                               <V76F12>
           COPY TR9QKREC.                                               <V73G34>
           COPY T3605REC.                                               <V76F02>
           COPY T1667REC.                                               <V76F06>
           COPY TV035REC.                                               <PHLRMS>
           COPY TV041REC.                                               <PHLRMS>
           COPY TV023REC.                                               <PHLRMS>
      /
           COPY ADOCSKM.
           COPY DESCSKM.
           COPY BBALSKM.
           COPY BATCSKM.                                                <V76F02>
           COPY CHQRSKM.                                                <V73G32>
           COPY GENLSKM.
           COPY ITEMSKM.
           COPY TRRNSKM.                                                <PHLRMS>
           COPY RCOLPNDSKM.                                             <PHLRMS>
           COPY TRRNTRNSKM.                                             <PHLRMS>
           COPY RCPYACTSKM.                                             <PHLRMS>
           COPY TTRCENQSKM.                                             <PHLRMS>
      **** COPY LETCUTISKM.                                             <FA3331>
           COPY RBNKSKM.
           COPY RBNKSEQSKM.                                             <V76F12>
           COPY RDOCSKM.
           COPY RCPTSKM.                                                <V76F12>
           COPY SANCSKR.                                                <V76F02>
           COPY ZPPRSKM.                                                <RC002>
           COPY ZPPRENQSKM.                                             <RC002>
           COPY CHDRLIFSKM.                                             <PHL109>
           COPY CLRRENQSKM.                                             <CS016>
           COPY RTRNDOCSKM.                                             <CS016>
      /
           COPY VARCOM.
           COPY TRNCURBALR.
           COPY SR289SCR.
      *
           COPY ADDRTRNREC.
           COPY ATREQREC.
           COPY BATCDORREC.                                             <V76F12>
           COPY CASHEDREC.
           COPY PAYEDTREC.                                              <V62P04>
           COPY CHKCURRREC.
           COPY CKTRDSCREC.
           COPY CLTRELNREC.
           COPY CLNTRLSREC.
           COPY DATCON1REC.
           COPY FDACCHKREC.
           COPY GETCLNTREC.
           COPY GLKEYREC.
           COPY GLSUBSTREC.
           COPY LETRQSTREC.
           COPY OPSTATSREC.
           COPY OPTSWCHREC.
           COPY PRTCLMFREC.                                             <V62P15>
           COPY RTVPRTFREC.
           COPY SYSERRREC.
           COPY SFLSAVEREC.                                             <V71F07>
           COPY SR22PSCR.                                               <V71F07>
      *
           COPY CASHTYPCPY.
           COPY DBCSTRNCPY.
           COPY FSUPFXCPY.
           COPY SMTPFXCPY.
           COPY SUBACCTCPY.
           COPY CSHRFCPY.
           COPY MSGBOXREC.                                              <V6F114>
           COPY SAVCBOXREC.                                             <V66P11>
           COPY SUBACCBALR.                                             <V74G02>
           COPY ZRDECPLREC.                                             <V76F06>
           COPY RCTSTATCPY.                                             <V76F12>
           COPY SFTLOCKREC.                                             <V76F12>
      /
       LINKAGE SECTION.

           COPY WSSPCOMN.

           COPY WSSPDOCS.

           COPY SCRNPARAMS.

           COPY S2610SCR.
      /
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA
                                SCRN-SCREEN-PARAMS S2610-DATA-AREA
                                S2610-SUBFILE-AREA.

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
      *
      * If returning from an optional selection skip this section.
      *
           MOVE WSSP-DOCTYPE           TO S2610-DOCTYPE.                <FA3503>
           MOVE SPACES                 TO WSAA-POLNO.                   <PHLRMS>
                                                                        <FA3503>
           PERFORM 1220-GET-DOCTDESC.                                   <FA3503>
                                                                        <FA3503>
           MOVE WSAA-DOCTDESC          TO S2610-LONGDESC.               <FA3503>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
               GO TO 1090-EXIT
           END-IF.
           IF  WSSP-SBMACTION          = 'A'                            <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Initialize temp array                                        <PHLRMS>
      *                                                                 <PHLRMS>
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 15           <PHLRMS>
                   MOVE SPACES         TO WSAA-ARR-SACSCODE  (IDX)      <PHLRMS>
                   MOVE SPACES         TO WSAA-ARR-SACSTYPW  (IDX)      <PHLRMS>
               END-PERFORM                                              <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           PERFORM 1180-RETRV-RBNK.                                     <V66P11>
           PERFORM 1400-KEEPS-SAVCBOX.                                  <V66P11>
           PERFORM 1100-INITIALIZATION.

           PERFORM 1200-LOAD-HEADER.

           PERFORM 1300-PROCESS-SUBFILE.

       1090-EXIT.
            EXIT.
      /
       1100-INITIALIZATION SECTION.
      *****************************
       1100-BEGIN.
      *
           INITIALIZE                     S2610-DATA-AREA.
           INITIALIZE                     S2610-SUBFILE-AREA.
           IF  WSSP-SBMACTION          = 'A'                            <PHLRMS>
               MOVE 'N'                TO S2610-CONFIRM                 <PHLRMS>
               MOVE 'Y'                TO S2610-CONFIRM-OUT  (PR)       <PHLRMS>
               MOVE 'Y'                TO S2610-CONFIRM-OUT  (ND)       <PHLRMS>
           ELSE                                                         <PHLRMS>
               MOVE SPACES             TO S2610-SFLAG                   <PHLRMS>
               MOVE 'Y'                TO S2610-CONFIRM                 <PHLRMS>
               MOVE 'Y'                TO S2610-CONFIRM-OUT  (PR)       <PHLRMS>
               MOVE 'Y'                TO S2610-CONFIRM-OUT  (ND)       <PHLRMS>
           END-IF.                                                      <PHLRMS>

           MOVE 'N'                    TO WSAA-FOUND-DUP.               <GP5152>
           MOVE ZEROES                 TO S2610-DOCSEQ
                                          S2610-SCRATE
                                          S2610-DOCORIGAMT
                                          S2610-DOCACCTAMT.

      * Initialize hidden fields
           MOVE SPACES                 TO S2610-GLSIGN
                                          S2610-GENLKEY
                                          S2610-TRANKEY
                                          S2610-CHDRKEY
                                          S2610-ORIGCUR
                                          S2610-SIGN
                                          S2610-PGMNAM
                                          S2610-TRANSCODE
                                          S2610-INDIC
                                          S2610-STATUZ
                                          S2610-ACCPFX
                                          S2610-ACCCOY
                                          S2610-ACCNUM
                                          S2610-EXTRA
                                          S2610-SOMEM.
           MOVE ZEROES                 TO S2610-IMBALAM
                                          S2610-TRANNO
                                          S2610-AMTRI
                                          S2610-CNTTOT.
      * Initialize WSAA varaibles
           MOVE SPACES                 TO WSAA-TEXT
                                          WSAA-DOCTDESC
                                          WSAA-BANK-FUND
                                          WSAA-SCRN-STATUZ
                                          WSAA-SCRN-SCRNAME
                                          WSAA-NEW-RECEIPT
                                          WSAA-PCP-MODE
                                          WSAA-FORMAT-FOR
                                          WSAA-PTCODE
                                          WSAA-PAYESEL
                                          WSAA-DOCTKEY
                                          WSAA-TRANKEY
                                          WSAA-PRINT-WH-CERT            <V65F04>
                                          WSAA-FOUND-CHK-BOX            <V66P11>
                                          WSAA-ENTER-CN-SCREEN          <V66P11>
                                          WSAA-REVISIT-CHK-BOX          <V66P11>
                                          WSAA-CN-CANCELLATION          <V66P11>
                                          WSAA-COVER-NOTE-ALLOWED       <V66P11>
                                          WSAA-PRINTED                  <V76F12>
                                          WSAA-ENTITY-123.

           MOVE ZEROES                 TO WSAA-TODAY
                                          WSAA-TIME
                                          WSAA-COUNT
                                          WSAA-TRANSEQ
                                          WSAA-SFL-CNT
                                          WSAA-SFL-RRN
                                          WSAA-SFL-END
                                          WSAA-SUBSCRIPT
                                          WSAA-DIFF
                                          WSAA-DISS-TOTAL
                                          WSAA-GLAMT-TOTAL              <FA4960>
                                          WSAA-GLIMBAL                  <FA4960>
                                          WSAA-DISS-COUNT
                                          WSAA-STORE-DOCORIGAMT         <FA2149>
                                          WSAA-IMBAL                    <V76F06>
                                          WSAA-IMBAL-N                  <V76F06>
                                          WSAA-LETC-RRN.
           MOVE 'Y'                    TO WSAA-FIRST-TIME.              <R2-296>
           MOVE 'Y'                    TO WSAA-FIRST-READ.              <V66P11>
           MOVE ZEROS                  TO WSAA-NUM-SELECTED.            <R2-296>
           MOVE 'N'                    TO WSAA-SR22P-NEW-REC.           <V71F07>
           MOVE 'N'                    TO WSAA-SR289-INDIC.             <GP4416>
      *                                                                 <V63P28>
      *---CHECK IF P2200UPKK SUBROUTINE EXISTS                          <V63P28>
           CALL 'CHKPGM'            USING WSAA-STATUZ                   <V63P28>
                                          WSAA-P2200UPKK                <V63P28>
                                          WSAA-OBJTYPE.                 <V63P28>
           IF WSAA-STATUZ               = O-K                           <V63P28>
              MOVE 'Y'                 TO WSAA-P2200UPKK-CALL           <V63P28>
             ELSE                                                       <V63P28>
              MOVE 'N'                 TO WSAA-P2200UPKK-CALL           <V63P28>
           END-IF.                                                      <V63P28>

      *---CHECK IF P2160UPVS SUBROUTINE EXISTS                          <V63P28>
           CALL 'CHKPGM'            USING WSAA-STATUZ                   <V63P28>
                                          WSAA-VAT-ON-SALES             <V63P28>
                                          WSAA-OBJTYPE.                 <V63P28>
           IF WSAA-STATUZ               = O-K                           <V63P28>
              MOVE 'Y'                 TO WSAA-VATS-CALL                <V63P28>
             ELSE                                                       <PHLRMS>
              MOVE 'N'                 TO WSAA-VATS-CALL                <V63P28>
           END-IF.                                                      <V63P28>

           MOVE SCLR                   TO SCRN-FUNCTION.
           PERFORM 9000-S2610-IO.

           MOVE 1                      TO SCRN-SUBFILE-RRN.

           MOVE WSSP-SBMACTION         TO WSAA-SBMACTION.
           MOVE WSSP-DOCTKEY           TO WSAA-DOCTKEY.
           MOVE WSAA-RECEIPT           TO WSAA-OLD-RECEIPT.
           MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
           MOVE WSSP-CLNTKEY           TO WSAA-CLNTKEY.
           MOVE WSSP-SUBFILE-END       TO WSAA-SR289-SFL-END.           <FA3701>
           MOVE WSSP-CHEQ-PRESFLAG     TO WSAA-POSTDTEFLG               <V76F12>
                                          S2610-POSTDTEFLG.             <V76F12>
           IF NOT-POSTDATED                                             <V76F12>
              MOVE 'Y'                 TO S2610-POSTDTEFLG-OUT(ND)      <V76F12>
           ELSE                                                         <V76F12>
              MOVE 'N'                 TO S2610-POSTDTEFLG-OUT(ND)      <V76F12>
           END-IF.                                                      <V76F12>

      * Hidden field for windowable field SACSTYPW
           MOVE WSKY-BATC-BATCTRCDE    TO S2610-TRANSCODE.

      * Hidden field for windowable field ENTITY
           MOVE WSAA-PROG              TO S2610-PGMNAM.

      * Get today's date and time.
           MOVE TDAY                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.
           MOVE DTC1-INT-DATE          TO WSAA-TODAY.
           CALL 'GETTIM'            USING WSAA-TIME.

      * Set ON Non-Display for "Imbalance" , amount and curreny fields.
           MOVE 'Y'                    TO S2610-IMBALAM-OUT (ND).

           PERFORM 1110-INIT-OPTSWCH.

           PERFORM 1120-READ-T3711.
           MOVE 'N'                    TO WSAA-FIRST-WARNING.           <R2-292>

           PERFORM 1130-READ-T3688.

      * Get fund code of the bankcode's GL Account
           PERFORM 1140-FUND-APPLICABLE.

      * Get fund code of the bankcode's GL Account                      <V76F13>
           PERFORM 1140A-GET-CC-GENLKEY.                                <V76F13>
                                                                        <V76F13>
           PERFORM 1150-READ-T3676.

           PERFORM 1160-READ-TR386.

           PERFORM 1170-LETTER-PROCESSING.

      * Retrieve RBNK to get the new receipt number
      **** PERFORM 1180-RETRV-RBNK.                                     <V66P11>
           MOVE RBNK-RDOCNUM           TO WSAA-NEW-RECEIPT.
                                                                        <V64F13>
      * Retrive Debtor Marry Indicator entered in SR289.                <V64F13>
           MOVE RBNK-MARRYFLAG         TO WSAA-MARRYFLAG.               <V64F13>

      * Retrive RBNK Post dated flag indicator                          <V76F12>
      * Its important to save the orginal postdated status of the       <V76F12>
      * receipt, incase it gets converted to 'regular' receipt          <V76F12>
      * it would know if it will save to RTRN and delete records        <V76F12>
      * from mirror file RCPT.                                          <V76F12>
           MOVE RBNK-POSTDTEFLG        TO WSAA-RBNK-POSTDTEFLG.         <V76F12>
                                                                        <V76F12>
           PERFORM 1190-INIT-TRNCURBAL.
           PERFORM A100-READ-TR9QK.                                     <V73G34>
           PERFORM A300-READ-T3605.                                     <V76F02>
           PERFORM A400-DISPLAY-USERNAME.                               <V76F02>
           PERFORM A500-READ-TR29S.                                     <V76F12>

       1100-EXIT.
            EXIT.
      /
       1110-INIT-OPTSWCH SECTION.
      ***************************
       1110-BEGIN.
      *
      * Retrieve Line Select and Function Key switching options.
      *
           MOVE INIT                   TO OPTS-FUNCTION.
           MOVE WSAA-PROG              TO OPTS-CALLING-PROG.
           MOVE WSAA-TODAY             TO OPTS-DTEEFF.
           MOVE WSSP-COMPANY           TO OPTS-COMPANY.
           MOVE WSSP-LANGUAGE          TO OPTS-LANGUAGE.
           MOVE WSSP-TRANID            TO VRCM-TRANID.
           MOVE VRCM-USER              TO OPTS-USER.

           CALL 'OPTSWCH'           USING OPTSWCH-REC
                                          WSSP-SEC-PROGS
                                          WSSP-SEC-ACTNS
                                          WSSP-PROGRAM-PTR
                                          WSSP-FLAG.

           IF OPTS-STATUZ              NOT = O-K
              MOVE 'INIT'              TO SYSR-FUNCTION
              MOVE OPTS-STATUZ         TO SYSR-DBIO-STATUZ
                                          SYSR-STATUZ
              MOVE 'OPTSWCH'           TO SYSR-IOMOD
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ZEROES                 TO WSAA-IX.
           PERFORM VARYING WSAA-COUNT FROM 3 BY 1
                   UNTIL   WSAA-COUNT > WSAA-MAX-OPT

              PERFORM 1111-MOVE-OPTSDSC

           END-PERFORM.
                                                                        <V66P11>
           IF  S2610-XOPTDSC            = SPACES                        <V66P11>
               MOVE 'Y'                TO S2610-XOPTIND-OUT(PR)         <V66P11>
               MOVE 'Y'                TO S2610-XOPTIND-OUT(ND)         <V66P11>
           ELSE                                                         <V66P11>
               IF  SBM-INQUIRY                                          <V66P11>
               OR  SBM-CANCELLATION                                     <V66P11>
               OR  SBM-PARTIAL-CANCEL                                   <V66P11>
               OR (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                   <V76F12>
                   SBM-MODIFY)                                          <V76F12>
                   IF  S2610-XOPTIND    = SPACES                        <V66P11>
                       MOVE 'Y'        TO S2610-XOPTIND-OUT(PR)         <V66P11>
                   END-IF                                               <V66P11>
               ELSE                                                     <V66P11>
                   MOVE 'N'            TO S2610-XOPTIND-OUT(PR)         <V66P11>
               END-IF                                                   <V66P11>
               MOVE 'N'                TO S2610-XOPTIND-OUT(ND)         <V66P11>
           END-IF.                                                      <V66P11>
                                                                        <V66P11>
           IF  SBM-CANCELLATION                                         <V66P11>
           OR  SBM-PARTIAL-CANCEL                                       <V66P11>
               IF  S2610-XOPTIND        = '+'                           <V66P11>
                   MOVE 'Y'            TO WSAA-ENTER-CN-SCREEN          <V66P11>
               END-IF                                                   <V66P11>
           END-IF.                                                      <V66P11>
                                                                        <V66P11>
       1110-EXIT.
            EXIT.
      /
       1111-MOVE-OPTSDSC SECTION.
      ***************************
       1111-BEGIN.
           EVALUATE OPTS-TYPE (WSAA-COUNT)
              WHEN 'L'
                ADD 1                  TO WSAA-IX
                IF OPTS-NO (WSAA-COUNT) = 2
                   IF SBM-PARTIAL-CANCEL
                      MOVE OPTS-DSC (WSAA-COUNT)
                                           TO S2610-OPTDSC (WSAA-IX)
                   END-IF
                ELSE
      ****      IF OPTS-CODE (WSAA-COUNT)   = 'WH'                      <V65F04>
      ****         MOVE SPACES             TO S2610-OPTDSC (WSAA-IX)    <V65F04>
      ****      ELSE                                                    <V65F04>
                IF OPTS-CODE (WSAA-COUNT)                               <V65P23>
                                        NOT = 'WH' AND 'VS' AND 'VP'    <V65P23>
                                        AND 'PS'                        <V72P02>
                   MOVE OPTS-DSC (WSAA-COUNT)
                                           TO S2610-OPTDSC (WSAA-IX)
                END-IF                                                  <V65F04>
                END-IF
              WHEN 'X'                                                  <V66P11>
                MOVE OPTS-DSC (WSAA-COUNT) TO S2610-XOPTDSC             <V66P11>
                MOVE OPTS-IND (WSAA-COUNT) TO S2610-XOPTIND             <V66P11>
           END-EVALUATE.
       1111-EXIT.
            EXIT.
      /
       1120-READ-T3711 SECTION.
      *************************
       1120-BEGIN.
      * Read T3711 to determine if TPA processing indicator is on

           INITIALIZE                     ITEM-DATA-AREA.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.
           MOVE T3711                  TO ITEM-ITEMTABL.
           MOVE WSSP-COMPANY           TO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.

           IF ITEM-STATUZ           NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA           TO T3711-T3711-REC.
       1120-EXIT.
            EXIT.
      /
       1130-READ-T3688 SECTION.
      *************************
       1130-BEGIN.

           INITIALIZE                     ITEM-DATA-KEY.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3688                  TO ITEM-ITEMTABL.
           MOVE WSSP-BANKCODE          TO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.
           IF  ITEM-STATUZ          NOT = O-K
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA           TO T3688-T3688-REC.
       1130-EXIT.
            EXIT.
      /
       1140-FUND-APPLICABLE SECTION.
      ******************************
       1140-BEGIN.

           PERFORM 1141-CALL-FDACCHK.
      *
           IF FDAC-FUNDACCL-YES
              PERFORM 1142-GET-FUNDCODE
           END-IF.
       1140-EXIT.
           EXIT.
      *
       1141-CALL-FDACCHK SECTION.
      ***************************
       1141-BEGIN.
      *
           MOVE SPACES                TO FDAC-FDACCHK-REC.
           MOVE WSSP-FSUCO            TO FDAC-ITEMCOY.
           MOVE WSSP-COMPANY          TO FDAC-ITEMITEM.
           CALL 'FDACCHK'          USING FDAC-FDACCHK-REC
      *
           IF FDAC-STATUZ          NOT = O-K
              MOVE FDAC-STATUZ        TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
       1141-EXIT.
           EXIT.
      *
       1142-GET-FUNDCODE  SECTION.
      ****************************
       1142-BEGIN.
      *
           MOVE SPACES                 TO WSAA-BANK-FUND.
      *
           INITIALIZE                     GENL-DATA-AREA.
           MOVE 'GE'                   TO GENL-GENLPFX.
           MOVE WSSP-COMPANY           TO GENL-GENLCOY.
           MOVE WSSP-ORIGCCY           TO GENL-GENLCUR.
      *
           PERFORM 1143-GLSUBST-BANKCODE.
           MOVE GLSB-GLACCT            TO GENL-GENLCDE.
           PERFORM B1010-READR-GENL.
      *
           IF GENL-STATUZ               = O-K
              MOVE GENL-SCRA           TO WSAA-BANK-FUND
           END-IF.
      *
       1142-EXIT.
           EXIT.
      *
       1143-GLSUBST-BANKCODE SECTION.
      *******************************
       1143-BEGIN.
      *
           MOVE SPACES                 TO GLSB-GLSUBST-REC.
           MOVE 'SBBNK'                TO GLSB-FUNCTION.
           MOVE WSSP-COMPANY           TO GLSB-COMPANY.
                                                                        <V76F13>
           IF WSSP-DOCTYPE              = CSTP-CREDIT-CARD-VAL          <V76F13>
              MOVE T3688-GLMAP-09      TO GLSB-GLACCT                   <V76F13>
           ELSE                                                         <V76F13>
           MOVE T3688-GLMAP-01         TO GLSB-GLACCT.
                                                                        <V76F13>
           MOVE WSSP-BANKCODE          TO GLSB-BANKCODE.
           MOVE WSSP-BRANCH            TO GLSB-BRANCH.
           MOVE WSSP-ORIGCCY           TO GLSB-CURRENCY.
           CALL 'GLSUBST'           USING GLSB-GLSUBST-REC.

           IF GLSB-STATUZ           NOT = O-K
              MOVE GLSB-GLSUBST-REC    TO SYSR-PARAMS
              MOVE GLSB-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

      *
       1143-EXIT.
           EXIT.
      /                                                                 <V76F13>
       1140A-GET-CC-GENLKEY SECTION.                                    <V76F13>
      ******************************                                    <V76F13>
       1141A-BEGIN.                                                     <V76F13>
           IF WSSP-DOCTYPE          NOT = CSTP-CREDIT-CARD-VAL          <V76F13>
              GO TO 1149A-EXIT.                                         <V76F13>
                                                                        <V76F13>
           MOVE WSSP-GENLKEY           TO WSAA-GENLKEY.                 <V76F13>
           MOVE T3688-GLMAP (9)        TO WSKY-GENL-GENLCDE.            <V76F13>
           MOVE T3688-SIGN  (9)        TO WSSP-SIGN.                    <V76F13>
                                                                        <V76F13>
           MOVE WSAA-GENLKEY           TO WSSP-GENLKEY.                 <V76F13>
       1149A-EXIT.                                                      <V76F13>
           EXIT.                                                        <V76F13>
      /
       1150-READ-T3676 SECTION.
      *************************
       1150-BEGIN.

           INITIALIZE                     ITEM-DATA-AREA.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.
           MOVE T3676                  TO ITEM-ITEMTABL.
           MOVE WSSP-DOCTYPE           TO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.

           MOVE ITEM-GENAREA           TO T3676-T3676-REC.
       1150-EXIT.
            EXIT.
      /
       1160-READ-TR386 SECTION.
      *************************
       1160-BEGIN.
      * Read TR386 table to get screen literals
           INITIALIZE                     ITEM-DATA-AREA.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.
           MOVE TR386                  TO ITEM-ITEMTABL.
           MOVE SPACES                 TO ITEM-ITEMITEM.
           STRING WSSP-LANGUAGE
                  WSAA-PROG
           DELIMITED BY SIZE         INTO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.

           IF ITEM-STATUZ           NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA           TO TR386-TR386-REC.
       1160-EXIT.
            EXIT.
      /
       1170-LETTER-PROCESSING SECTION.
      ********************************
       1170-BEGIN.

           INITIALIZE                     ITEM-DATA-AREA.               <PCPRT2>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <PCPRT2>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY                  <PCPRT2>
           MOVE TR384                  TO ITEM-ITEMTABL.                <PCPRT2>
           MOVE S2610-TRANSCODE        TO WSAA-TRANSCODE.               <PCPRT2>
           MOVE WSAA-TR384-ITEMITEM    TO ITEM-ITEMITEM.                <PCPRT2>
           PERFORM B1000-READR-ITEM.                                    <PCPRT2>
                                                                        <PCPRT2>
           IF  ITEM-STATUZ             = MRNF                           <PCPRT2>
               MOVE 'N'                TO WSAA-PCPRT-ENABLE             <PCPRT2>
               MOVE SPACES             TO T2634-T2634-REC               <PCPRT2>
               GO TO 1170-EXIT                                          <PCPRT2>
           END-IF.                                                      <PCPRT2>
      *                                                                 <PCPRT2>
      *    If item exist, this letter type has been defined for the     <PCPRT2>
      *    product which we are running for.                            <PCPRT2>
      *                                                                 <PCPRT2>
           MOVE 'Y'                    TO WSAA-PCPRT-ENABLE.            <PCPRT2>
      *                                                                 <PCPRT2>
      *    Now the read on T2634 is to obtain the formatting subroutine <PCPRT2>
      *    which can be using the dummy item with all '*'.              <PCPRT2>
      *                                                                 <PCPRT2>
           INITIALIZE                     ITEM-DATA-AREA.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY
           MOVE T2634                  TO ITEM-ITEMTABL.
      **** MOVE WSAA-RECEIPT-LETTER    TO ITEM-ITEMITEM.                <PCPRT2>
           MOVE ALL '*'                TO ITEM-ITEMITEM.                <PCPRT2>
           PERFORM B1000-READR-ITEM.

           IF  ITEM-STATUZ             = MRNF
               MOVE 'N'                TO WSAA-PCPRT-ENABLE
               MOVE SPACES             TO T2634-T2634-REC
               GO TO 1170-EXIT
           END-IF.

           MOVE ITEM-GENAREA           TO T2634-T2634-REC.
      *
      *    Check to see if this letter type has been defined for the
      *    product which we are running for.
      *
           IF  T2634-PRTROUTE(02)   NOT = SPACES
           AND T2634-SUBRNAME(02)   NOT = SPACES
               MOVE T2634-SUBRNAME(02) TO WSAA-FORMAT-FOR
      ****     MOVE 'Y'                TO WSAA-PCPRT-ENABLE             <PCPRT2>
           ELSE
               MOVE SPACES             TO WSAA-FORMAT-FOR
      ****     MOVE 'N'                TO WSAA-PCPRT-ENABLE             <PCPRT2>
           END-IF.

       1170-EXIT.
            EXIT.
      /
       1180-RETRV-RBNK SECTION.
      *************************
       1180-BEGIN.
      *
           MOVE RBNKREC                TO RBNK-FORMAT.
           MOVE RETRV                  TO RBNK-FUNCTION.
           CALL 'RBNKIO'            USING RBNK-PARAMS.

           IF  RBNK-STATUZ          NOT = O-K
               MOVE RBNK-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
     *
           MOVE KEEPS                  TO RBNK-FUNCTION.
           MOVE RBNKREC                TO RBNK-FORMAT.
           CALL 'RBNKIO'            USING RBNK-PARAMS.
           IF RBNK-STATUZ          NOT = O-K
              MOVE RBNK-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
       1180-EXIT.
            EXIT.
      /
       1190-INIT-TRNCURBAL SECTION.
      *****************************
       1190-BEGIN.
           MOVE 'INIT '                TO TRNC-FUNCTION.

           CALL 'TRNCURBAL'         USING TRNC-TRNCURBAL-REC.
           IF TRNC-STATUZ           NOT = O-K
              MOVE 'TRNCURBAL'         TO SYSR-SUBRNAME
              MOVE TRNC-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE WSSP-ACCTCCY           TO TRNC-ACCTCURR.
       1190-EXIT.
            EXIT.
      /
       1200-LOAD-HEADER SECTION.
      **************************
       1200-BEGIN.
      *
           PERFORM 1210-RETRIEVE-HEADER-DESC.

           IF DESC-STATUZ              = MRNF
              MOVE ALL '?'             TO S2610-HEADER
           ELSE
              MOVE DESC-LONGDESC       TO S2610-HEADER
           END-IF.

           MOVE WSAA-NEW-RECEIPT       TO S2610-DOCNO
           MOVE WSSP-DOCTYPE           TO S2610-DOCTYPE.

           PERFORM 1220-GET-DOCTDESC.

           MOVE WSAA-DOCTDESC          TO S2610-LONGDESC.
           MOVE WSSP-TRANDATE          TO S2610-TRANDATEX.
      * If Action A (Create Receipt - Cash not Banked):                 <RC002>
      *   Transaction Date: use Deposite date instead of Receipt        <RC002>
      *   creation Date (Apply the Pre-printed Receipt).                <RC002>
           IF WSAA-SBMACTION            = 'A'                           <RC002>
              AND  WSSP-CURRFROM    NOT = ZEROES                        <RC002>
               MOVE  WSSP-CURRFROM     TO S2610-TRANDATEX.              <RC002>
           MOVE ZEROS                  TO WSSP-CURRFROM.                <RC002>
       1200-EXIT.
            EXIT.
      /
       1210-RETRIEVE-HEADER-DESC SECTION.
      ***********************************
       1210-BEGIN.
      *
           INITIALIZE                     DESC-DATA-AREA.
           MOVE SMTP-ITEM              TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T1688                  TO DESC-DESCTABL.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE WSKY-BATC-BATCTRCDE    TO DESC-DESCITEM.
           PERFORM B1020-READR-DESC.

       1210-EXIT.
            EXIT.
      /
       1220-GET-DOCTDESC SECTION.
      ***************************
       1220-BEGIN.
           INITIALIZE                     DESC-DATA-AREA.
           MOVE SMTP-ITEM              TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE T3676                  TO DESC-DESCTABL.
           MOVE WSSP-DOCTYPE           TO DESC-DESCITEM.
           MOVE SPACES                 TO DESC-ITEMSEQ.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           PERFORM B1020-READR-DESC.

           IF DESC-STATUZ               = MRNF
              MOVE ALL '?'             TO WSAA-DOCTDESC
           ELSE
              MOVE DESC-LONGDESC       TO WSAA-DOCTDESC
           END-IF.
       1220-EXIT.
            EXIT.
      /
       1300-PROCESS-SUBFILE SECTION.
      ******************************
       1300-BEGIN.
           IF SBM-CREATION
             PERFORM 1310-LOAD-NEW-SUBFILE
           ELSE
             PERFORM 1320-LOAD-ADOC-TO-SUBFILE
           END-IF.
       1300-EXIT.
            EXIT.
      /
       1310-LOAD-NEW-SUBFILE SECTION.
      *******************************
       1310-BEGIN.

           PERFORM 1311-LOAD-FIRST-DISSECTION.
           MOVE 1                      TO WSAA-SFL-CNT
                                          WSAA-TRANSEQ.
           PERFORM 1313-LOAD-DLF-DISSECTION.                            <V76F07>
           PERFORM 1312-LOAD-BLANK-SUBFILE UNTIL
                              WSAA-SFL-CNT > S2610-SUBFILE-PAGE.

           MOVE 'Y'                    TO SCRN-SUBFILE-MORE.

       1310-EXIT.
            EXIT.
      /
       1311-LOAD-FIRST-DISSECTION SECTION.
      ************************************
       1311-BEGIN.
      *
      *  The very first dissection is reserved for the header
      *  information (except for Journal transactions).
      *
           MOVE WSSP-PTCODE            TO S2610-FSACSCODE.
           MOVE WSSP-DOCTYPE           TO S2610-FSACSTYP.
           MOVE WSSP-PAYESEL           TO S2610-MLENTITY.
           MOVE WSSP-PAYENME           TO S2610-TRANDSC.
           MOVE ZEROES                 TO S2610-DOCSEQ.
           MOVE WSSP-DOCORIGAMT        TO S2610-DOCORIGAMT.
           MOVE WSSP-ORIGCCY           TO S2610-ORIGCURR.
           MOVE WSSP-SCRATE            TO S2610-SCRATE.
           MOVE WSSP-DOCACCTAMT        TO S2610-DOCACCTAMT.
           MOVE WSSP-ACCTCCY           TO S2610-ACCTCCY.
                                                                        <V76F12>
      * Set Postdated field                                             <V76F12>
           MOVE WSSP-CHEQ-PRESFLAG     TO S2610-POSTDTEFLG              <V76F12>
                                          WSAA-POSTDTEFLG.              <V76F12>
           IF NOT-POSTDATED                                             <V76F12>
              MOVE 'Y'                 TO S2610-POSTDTEFLG-OUT(ND)      <V76F12>
           ELSE                                                         <V76F12>
              MOVE 'N'                 TO S2610-POSTDTEFLG-OUT(ND)      <V76F12>
           END-IF.                                                      <V76F12>

       1311-EXIT.
            EXIT.
      /                                                                 <V76F07>
       1313-LOAD-DLF-DISSECTION SECTION.                                <V76F07>
      **********************************                                <V76F07>
       1313-BEGIN.                                                      <V76F07>
      *--Default from T3605 from if Policy is entered in submenu.       <V76F07>
           IF SCRN-DEVICE-IND           = '*RMT'                        <V76F07>
           OR WSSP-CHDRKEY              = SPACES                        <V76F07>
              MOVE SCRN-SUBFILE-END    TO WSSP-SUBFILE-END              <V76F07>
              GO TO 1313-EXIT.                                          <V76F07>
                                                                        <V76F07>
           MOVE ZEROES                 TO S2610-SELECT-NUMBER.          <V76F07>
           MOVE T3605-SACSCODE         TO S2610-SACSCODE.               <V76F07>
           MOVE T3605-SACSTYP          TO S2610-SACSTYPW.               <V76F07>
           MOVE WSSP-CHDR-CHDRNUM      TO S2610-ENTITY.                 <V76F07>
           MOVE SPACES                 TO S2610-TRANDESC.               <V76F07>
           MOVE SPACES                 TO S2610-DISACC.                 <V76F07>
           MOVE WSAA-TRANSEQ           TO S2610-TRANSEQ.                <V76F07>
           MOVE ZEROES                 TO S2610-ORIGAMT.                <V76F07>
           MOVE SPACES                 TO S2610-BCURR.                  <V76F07>
           MOVE ZEROES                 TO S2610-SLRATE.                 <V76F07>
           MOVE ZEROES                 TO S2610-ACCTAMT.                <V76F07>
           MOVE WSSP-ACCTCCY           TO S2610-GENLCUR.                <V76F07>
           MOVE ZEROS                  TO S2610-AMNT                    <V76F07>
                                          S2610-CNT.                    <V76F07>
           MOVE SPACES                 TO S2610-INDIC.                  <V76F07>
           IF WSSP-SERVUNIT            = 'RN'                           <CS009>
           AND ( WSSP-SBMACTION        = 'A' OR 'B' )                   <CS009>
              MOVE 'LP'                TO S2610-SACSCODE                <CS009>
              MOVE 'S'                 TO S2610-SACSTYPW                <CS009>
              MOVE WSSP-DOCORIGAMT     TO S2610-ORIGAMT                 <CS009>
           END-IF.                                                      <CS009>
                                                                        <V76F07>
           ADD 1                       TO WSAA-TRANSEQ                  <V76F07>
                                          WSAA-SFL-CNT.                 <V76F07>
           MOVE SADD                   TO SCRN-FUNCTION.                <V76F07>
           PERFORM 9000-S2610-IO.                                       <V76F07>
       1313-EXIT.                                                       <V76F07>
           EXIT.                                                        <V76F07>
                                                                        <V76F07>
      /
       1312-LOAD-BLANK-SUBFILE SECTION.
      *********************************
       1312-BEGIN.
      * Do not load blank subfile lines if called from remote device
      * i.e. if called from 3r.
           IF SCRN-DEVICE-IND          = '*RMT'
      ***     MOVE 2                   TO WSSP-SUBFILE-END      <V62P04><WEBNQ1>
              MOVE SCRN-SUBFILE-END    TO WSSP-SUBFILE-END              <WEBNQ1>
              GO TO 1312-CONT
           END-IF.

           MOVE ZEROES                 TO S2610-SELECT-NUMBER.
           MOVE SPACES                 TO S2610-SACSCODE.
           MOVE SPACES                 TO S2610-SACSTYPW.
           MOVE SPACES                 TO S2610-ENTITY.
           MOVE SPACES                 TO S2610-TRANDESC.
           MOVE SPACES                 TO S2610-DISACC.
           MOVE WSAA-TRANSEQ           TO S2610-TRANSEQ.
           MOVE ZEROES                 TO S2610-ORIGAMT.
           MOVE SPACES                 TO S2610-BCURR.
           MOVE ZEROES                 TO S2610-SLRATE.
           MOVE ZEROES                 TO S2610-ACCTAMT.
           MOVE WSSP-ACCTCCY           TO S2610-GENLCUR.
           MOVE ZEROS                  TO S2610-AMNT                    <R2-296>
                                          S2610-CNT.                    <R2-296>
           MOVE SPACES                 TO S2610-INDIC.                  <R2-296>

           ADD 1                       TO WSAA-TRANSEQ.
           MOVE SADD                   TO SCRN-FUNCTION.
           PERFORM 9000-S2610-IO.

       1312-CONT.
           ADD 1                       TO WSAA-SFL-CNT.
       1312-EXIT.
            EXIT.
      /
       1320-LOAD-ADOC-TO-SUBFILE SECTION.
      ***********************************
       1320-BEGIN.

      * Load the first RTRN to dissection header (non-subfile)
           INITIALIZE                     ADOC-DATA-AREA.
           MOVE PRFX-CASH              TO ADOC-RTRN-RDOCPFX.
           MOVE WSSP-COMPANY           TO ADOC-RTRN-RDOCCOY.
           MOVE WSAA-RECEIPT           TO ADOC-RTRN-RDOCNUM.
           MOVE ZEROES                 TO ADOC-RTRN-TRANNO.
           MOVE ZEROES                 TO ADOC-RTRN-JRNSEQ.
           MOVE RTRNREC                TO ADOC-FORMAT.
           MOVE BEGN                   TO ADOC-FUNCTION.
           PERFORM 1321-LOAD-FIRST-ADOC.
           IF ADOC-STATUZ               = ENDP                          <V76F12>
              PERFORM 1600-LOAD-RCPT-TO-SUBFILE                         <V76F12>
              GO TO 1320-EXIT                                           <V76F12>
           END-IF.                                                      <V76F12>

      * Load the subfile with next existing RTRN records.
           MOVE NEXTR                  TO ADOC-FUNCTION.
           MOVE SPACES                 TO ADOC-FORMAT.
           MOVE ZEROES                 TO WSAA-KFK-RRN                  <V62P04>
           PERFORM 1323-LOAD-EXISTING-ADOC
                                    UNTIL ADOC-STATUZ = ENDP.

           MOVE 1                      TO SCRN-SUBFILE-RRN.

           IF SBM-CANCELLATION   OR
              SBM-PARTIAL-CANCEL OR
              SBM-INQUIRY
           OR (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                       <V76F12>
               SBM-MODIFY)                                              <V76F12>
              MOVE 'N'                 TO SCRN-SUBFILE-MORE
           ELSE
              MOVE 'Y'                 TO SCRN-SUBFILE-MORE
           END-IF.
      *
       1320-EXIT.
            EXIT.
      /
       1321-LOAD-FIRST-ADOC SECTION.
      ******************************
       1321-BEGIN.
           CALL 'ADOCIO'            USING ADOC-PARAMS.
           IF ADOC-STATUZ           NOT = O-K AND ENDP
              MOVE ADOC-PARAMS         TO SYSR-PARAMS
              MOVE ADOC-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
           IF ADOC-STATUZ               = ENDP         OR
              ADOC-RTRN-RDOCPFX     NOT = PRFX-CASH    OR
              ADOC-RTRN-RDOCCOY     NOT = WSSP-COMPANY OR
              ADOC-RTRN-RDOCNUM     NOT = WSAA-RECEIPT OR
              ADOC-RTRN-JRNSEQ      NOT = ZEROES
              MOVE PRFX-CASH           TO ADOC-RTRN-RDOCPFX
              MOVE WSSP-COMPANY        TO ADOC-RTRN-RDOCCOY
              MOVE WSAA-RECEIPT        TO ADOC-RTRN-RDOCNUM
              MOVE ZEROES              TO ADOC-RTRN-TRANNO
              MOVE ZEROES              TO ADOC-RTRN-JRNSEQ
              MOVE ADOC-PARAMS         TO SYSR-PARAMS
              MOVE ADOC-STATUZ         TO SYSR-STATUZ
              MOVE ENDP                TO ADOC-STATUZ                   <V76F12>
              GO 1321-EXIT                                              <V76F12>
      *       PERFORM 600-FATAL-ERROR                                   <V76F12>
           END-IF.
                                                                        <CS016>
           IF WSAA-SBMACTION           = 'M'                            <CS016>
              PERFORM 1311-LOAD-FIRST-DISSECTION                        <CS016>
              GO TO 1321-EXIT                                           <CS016>
           END-IF.                                                      <CS016>

           IF SBM-PARTIAL-CANCEL
              PERFORM 1311-LOAD-FIRST-DISSECTION
           ELSE
              PERFORM 1322-MOVE-FIRST-ADOC
           END-IF.
      *
       1321-EXIT.
            EXIT.
      /
       1322-MOVE-FIRST-ADOC SECTION.
      ******************************
       1322-BEGIN.
           MOVE ADOC-RTRN-SACSCODE     TO S2610-FSACSCODE.
           MOVE ADOC-RTRN-SACSTYP      TO S2610-FSACSTYP.
           MOVE ADOC-RTRN-RLDGACCT     TO S2610-MLENTITY.
           MOVE ADOC-RTRN-TRANDESC     TO S2610-TRANDSC.
           MOVE ADOC-RTRN-JRNSEQ       TO WSAA-JRNSEQ.
           MOVE WSAA-JRNSEQ            TO S2610-DOCSEQ.
           MOVE ADOC-RTRN-ORIGAMT      TO S2610-DOCORIGAMT.
           MOVE ADOC-RTRN-ORIGCCY      TO S2610-ORIGCURR.
           MOVE ADOC-RTRN-CRATE        TO S2610-SCRATE.
           MOVE ADOC-RTRN-ACCTAMT      TO S2610-DOCACCTAMT.
           MOVE ADOC-RTRN-GENLCUR      TO S2610-ACCTCCY.

      * Keep this BATCKEY for printing purpose only during Inquiry
           IF SBM-INQUIRY
           OR (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                       <V76F12>
               SBM-MODIFY)                                              <V76F12>
              MOVE SMTP-BATC           TO WSAA-SAVE-BATCPFX
              MOVE ADOC-RTRN-BATCCOY   TO WSAA-SAVE-BATCCOY
              MOVE ADOC-RTRN-BATCBRN   TO WSAA-SAVE-BATCBRN
              MOVE ADOC-RTRN-BATCACTYR TO WSAA-SAVE-BATCACTYR
              MOVE ADOC-RTRN-BATCACTMN TO WSAA-SAVE-BATCACTMN
              MOVE ADOC-RTRN-BATCTRCDE TO WSAA-SAVE-BATCTRCDE
              MOVE ADOC-RTRN-BATCBATCH TO WSAA-SAVE-BATCBATCH
           END-IF.
      *                                                                 <PHLRMS>
      * For cancellation, reverse the amount.
      *
           IF SBM-CANCELLATION
              COMPUTE S2610-DOCORIGAMT    = S2610-DOCORIGAMT * -1
              COMPUTE S2610-DOCACCTAMT    = S2610-DOCACCTAMT * -1
           END-IF.
       1322-EXIT.
            EXIT.
      /
       1323-LOAD-EXISTING-ADOC SECTION.
      ********************************
       1323-BEGIN.
      *
           CALL 'ADOCIO'            USING ADOC-PARAMS.
           IF ADOC-STATUZ           NOT = O-K AND ENDP
              MOVE ADOC-PARAMS         TO SYSR-PARAMS
              MOVE ADOC-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
           IF ADOC-STATUZ               = ENDP         OR
              ADOC-RTRN-RDOCPFX     NOT = PRFX-CASH    OR
              ADOC-RTRN-RDOCCOY     NOT = WSSP-COMPANY OR
              ADOC-RTRN-RDOCNUM     NOT = WSAA-RECEIPT
              MOVE ENDP                TO ADOC-STATUZ
              GO TO 1323-EXIT
           END-IF.

      * Do not display Invisible Dissections, check flag on T3719
           MOVE SPACES                 TO WSKY-ITEM-ITEMITEM.
           STRING WSKY-BATC-BATCTRCDE
                  ADOC-RTRN-SACSCODE
                  ADOC-RTRN-SACSTYP
           DELIMITED BY SIZE         INTO WSKY-ITEM-ITEMITEM.

           PERFORM 1324-READ-T3719.

           IF T3719-IND-01          NOT = SPACES AND
              T3719-IND-02          NOT = SPACES
              GO TO 1323-NEXTR
           END-IF.
      *
           MOVE ZEROES                 TO S2610-SELECT-NUMBER.
           MOVE ADOC-RTRN-SACSCODE     TO S2610-SACSCODE.
           MOVE ADOC-RTRN-SACSTYP      TO S2610-SACSTYPW.
           MOVE ADOC-RTRN-RLDGACCT     TO S2610-ENTITY.
           MOVE ADOC-RTRN-TRANDESC     TO S2610-TRANDESC.
           MOVE ADOC-RTRN-ACCNO        TO S2610-DISACC.
           MOVE ADOC-RTRN-JRNSEQ       TO WSAA-JRNSEQ.
           MOVE WSAA-JRNSEQ            TO S2610-TRANSEQ.
           MOVE ADOC-RTRN-ORIGAMT      TO S2610-ORIGAMT.
           MOVE ADOC-RTRN-ORIGCCY      TO S2610-BCURR.
           MOVE ADOC-RTRN-CRATE        TO S2610-SLRATE.
           MOVE ADOC-RTRN-ACCTAMT      TO S2610-ACCTAMT.
           MOVE ADOC-RTRN-GENLCUR      TO S2610-GENLCUR.

      * For cancellation, reverse the amount.
           IF SBM-CANCELLATION   OR
              SBM-PARTIAL-CANCEL
              COMPUTE S2610-ORIGAMT    = S2610-ORIGAMT * -1
              COMPUTE S2610-ACCTAMT    = S2610-ACCTAMT * -1
           END-IF.

      * Move values to hidden fields.
           MOVE ADOC-RTRN-RLDGPFX      TO WSAA-TRANPFX.
           MOVE ADOC-RTRN-RLDGCOY      TO WSAA-TRANCOY.
           MOVE ADOC-RTRN-RLDGPFX      TO WSAA-WTRANPFX.                <FA3384>
           MOVE ADOC-RTRN-RLDGCOY      TO WSAA-WTRANCOY.                <FA3384>
           MOVE ADOC-RTRN-RLDGACCT     TO WSAA-ENTITY.
           MOVE WSAA-TRANKEY           TO S2610-TRANKEY.
           MOVE SCRN-SUBFILE-RRN       TO WSAA-SUBFILE-RRN.             <V62P04>
           ADD 1                       TO WSAA-KFK-RRN.                 <V62P04>
           MOVE WSAA-KFK-RRN           TO S2610-KFKSEQ.                 <V62P04>
           MOVE ZEROS                  TO S2610-CNT.                    <R2-296>
           MOVE ADOC-RTRN-TRANNO       TO S2610-TRANNO.

      * If no edit routine exist on T3697 for the Subaccount Code
      * being processed, protect and non-display the SELECT field.
           MOVE SPACES                 TO WSKY-ITEM-ITEMITEM.
           STRING WSAA-PROG
                  S2610-SACSCODE
           DELIMITED BY SIZE         INTO WSKY-ITEM-ITEMITEM.

           PERFORM 1325-READ-T3697.

           IF T3697-VALPROG            = SPACES
              MOVE 'Y'                 TO S2610-SELNUM-OUT (PR)
              MOVE 'Y'                 TO S2610-SELNUM-OUT (ND)
           ELSE
              MOVE 'N'                 TO S2610-SELNUM-OUT (PR)
              MOVE 'N'                 TO S2610-SELNUM-OUT (ND)
           END-IF.

      * Protect screen fields
           IF SBM-PARTIAL-CANCEL
              PERFORM 1326-PROTECT-FIELDS
              MOVE 'N'                 TO S2610-ORIGAMT-OUT (PR)
                                          S2610-ACCTAMT-OUT (PR)
           ELSE
              PERFORM 1326-PROTECT-FIELDS
           END-IF.

           IF SBM-CANCELLATION                                          <FA3060>
              MOVE 'Y'                 TO S2610-SELNUM-OUT(PR)          <FA3060>
           END-IF.                                                      <FA3060>
                                                                        <CS016>
           IF WSAA-SBMACTION           = 'M'                            <CS016>
              MOVE 'N'                 TO S2610-TRANDESC-OUT(PR)        <CS016>
              MOVE 'Y'                 TO S2610-SELNUM-OUT(PR)          <CS016>
           END-IF.                                                      <CS016>
      *
           MOVE SADD                   TO SCRN-FUNCTION.
           PERFORM 9000-S2610-IO.
      *
       1323-NEXTR.
           MOVE NEXTR                  TO ADOC-FUNCTION.
           MOVE SPACES                 TO ADOC-FORMAT.
       1323-EXIT.
            EXIT.
      /
       1324-READ-T3719 SECTION.
      *************************
       1324-BEGIN.
           INITIALIZE                     ITEM-DATA-AREA.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.
           MOVE T3719                  TO ITEM-ITEMTABL.
           MOVE WSKY-ITEM-ITEMITEM     TO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.

           IF ITEM-STATUZ              = O-K
              MOVE ITEM-GENAREA        TO T3719-T3719-REC
           ELSE
              INITIALIZE                  T3719-T3719-REC
           END-IF.
       1324-EXIT.
            EXIT.
      /
       1325-READ-T3697 SECTION.
      *************************
       1325-BEGIN.
      *
           INITIALIZE                     ITEM-DATA-AREA.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.
           MOVE T3697                  TO ITEM-ITEMTABL.
           MOVE WSKY-ITEM-ITEMITEM     TO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.

           IF ITEM-STATUZ              = O-K
              MOVE ITEM-GENAREA        TO T3697-T3697-REC
           ELSE
              INITIALIZE                  T3697-T3697-REC
           END-IF.
       1325-EXIT.
            EXIT.
      /
       1326-PROTECT-FIELDS SECTION.
      *****************************
       1326-BEGIN.
           MOVE 'Y'                    TO S2610-SACSCODE-OUT (PR)
                                          S2610-SACSTYPW-OUT (PR)
                                          S2610-ENTITY-OUT   (PR)
                                          S2610-TRANDESC-OUT (PR)
                                          S2610-DISACC-OUT   (PR)
                                          S2610-TRANSEQ-OUT  (PR)
                                          S2610-ORIGAMT-OUT  (PR)
                                          S2610-BCURR-OUT    (PR)
                                          S2610-SLRATE-OUT   (PR)
                                          S2610-ACCTAMT-OUT  (PR)
                                          S2610-GENLCUR-OUT  (PR).
       1326-EXIT.
            EXIT.
      /
       1400-KEEPS-SAVCBOX SECTION.                                      <V66P11>
      ****************************                                      <V66P11>
       1410-START.                                                      <V66P11>
           MOVE RBNK-RDOCPFX           TO SVCB-NEW-RDOCPFX.             <V66P11>
           MOVE RBNK-RDOCCOY           TO SVCB-NEW-RDOCCOY.             <V66P11>
           MOVE RBNK-RDOCNUM           TO SVCB-NEW-RDOCNUM.             <V66P11>
           MOVE WSSP-RDOCPFX           TO SVCB-OLD-RDOCPFX.             <V66P11>
           MOVE WSSP-RDOCCOY           TO SVCB-OLD-RDOCCOY.             <V66P11>
           MOVE WSSP-RDOCNUM           TO SVCB-OLD-RDOCNUM.             <V66P11>
           MOVE WSSP-SBMACTION         TO SVCB-SBM-ACTION.              <V66P11>
           MOVE SPACES                 TO SVCB-IND-UPDATE.              <V66P11>
           MOVE KEEPS                  TO SVCB-FUNCTION.                <V66P11>
                                                                        <V66P11>
           CALL 'SAVCBOX'           USING SVCB-SAVCBOX-REC.             <V66P11>
           IF  SVCB-STATUZ          NOT = O-K                           <V66P11>
               MOVE SVCB-STATUZ        TO SYSR-STATUZ                   <V66P11>
               MOVE SVCB-SAVCBOX-REC   TO SYSR-PARAMS                   <V66P11>
               PERFORM 600-FATAL-ERROR                                  <V66P11>
           END-IF.                                                      <V66P11>
                                                                        <V66P11>
       1490-EXIT.                                                       <V66P11>
           EXIT.                                                        <V66P11>
                                                                        <V66P11>
       1500-RETRV-SAVCBOX SECTION.                                      <V66P11>
      ****************************                                      <V66P11>
       1510-START.                                                      <V66P11>
           MOVE RETRV                  TO SVCB-FUNCTION.                <V66P11>
                                                                        <V66P11>
           CALL 'SAVCBOX'           USING SVCB-SAVCBOX-REC.             <V66P11>
           IF  SVCB-STATUZ          NOT = O-K                           <V66P11>
               MOVE SVCB-STATUZ        TO SYSR-STATUZ                   <V66P11>
               MOVE SVCB-SAVCBOX-REC   TO SYSR-PARAMS                   <V66P11>
               PERFORM 600-FATAL-ERROR                                  <V66P11>
           END-IF.                                                      <V66P11>
                                                                        <V66P11>
       1590-EXIT.                                                       <V66P11>
           EXIT.                                                        <V66P11>
      /                                                                 <V76F12>
       1600-LOAD-RCPT-TO-SUBFILE SECTION.                               <V76F12>
      ***********************************                               <V76F12>
       1600-BEGIN.                                                      <V76F12>
           INITIALIZE                     RCPT-DATA-KEY.                <V76F12>
           MOVE PRFX-CASH              TO RCPT-RDOCPFX.                 <V76F12>
           MOVE WSSP-COMPANY           TO RCPT-RDOCCOY.                 <V76F12>
           MOVE WSAA-RECEIPT           TO RCPT-RDOCNUM.                 <V76F12>
           MOVE ZEROES                 TO RCPT-SEQNBR.                  <V76F12>
           MOVE RCPTREC                TO RCPT-FORMAT.                  <V76F12>
           MOVE BEGN                   TO RCPT-FUNCTION.                <V76F12>
           PERFORM 1610-LOAD-FIRST-RCPT.                                <V76F12>
                                                                        <V76F12>
           MOVE ZEROES                 TO WSAA-KFK-RRN                  <V76F12>
           MOVE NEXTR                  TO RCPT-FUNCTION.                <V76F12>
           PERFORM 1620-LOAD-EXISTING-RCPT                              <V76F12>
                                       UNTIL RCPT-STATUZ = ENDP.        <V76F12>
           IF WSAA-RBNK-POSTDTEFLG     = 'Y'                            <V76F12>
           AND NOT PRINTED                                              <V76F12>
           AND SBM-MODIFY                                               <V76F12>
              DIVIDE SCRN-SUBFILE-RRN  BY S2610-SUBFILE-PAGE            <V76F12>
                                       GIVING WSAA-NUMBER               <V76F12>
                                       REMAINDER WSAA-SFL-CNT           <V76F12>
                                                                        <V76F12>
              MOVE WSAA-SFL-CNT        TO WSAA-TRANSEQ                  <V76F12>
              PERFORM 1312-LOAD-BLANK-SUBFILE UNTIL                     <V76F12>
                          WSAA-SFL-CNT >= S2610-SUBFILE-PAGE            <V76F12>
                                                                        <V76F12>
              MOVE 'Y'                 TO SCRN-SUBFILE-MORE             <V76F12>
           ELSE                                                         <V76F12>
              MOVE 'N'                 TO SCRN-SUBFILE-MORE             <V76F12>
           END-IF.                                                      <V76F12>
       1600-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       1610-LOAD-FIRST-RCPT SECTION.                                    <V76F12>
      ******************************                                    <V76F12>
       1610-BEGIN.                                                      <V76F12>
           PERFORM C200-RCPTIO.                                         <V76F12>
                                                                        <V76F12>
           IF RCPT-STATUZ               = ENDP                          <V76F12>
           OR RCPT-RDOCPFX          NOT = PRFX-CASH                     <V76F12>
           OR RCPT-RDOCCOY          NOT = WSSP-COMPANY                  <V76F12>
           OR RCPT-RDOCNUM          NOT = WSAA-RECEIPT                  <V76F12>
           OR RCPT-SEQNBR           NOT = ZEROES                        <V76F12>
              MOVE RCPT-PARAMS         TO SYSR-PARAMS                   <V76F12>
              MOVE RCPT-STATUZ         TO SYSR-STATUZ                   <V76F12>
              PERFORM 600-FATAL-ERROR                                   <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE RCPT-SACSCODE          TO S2610-FSACSCODE.              <V76F12>
           MOVE RCPT-SACSTYP           TO S2610-FSACSTYP.               <V76F12>
           MOVE RCPT-RLDGACCT          TO S2610-MLENTITY.               <V76F12>
           MOVE RCPT-TRANDESC          TO S2610-TRANDSC.                <V76F12>
           MOVE RCPT-SEQNBR            TO WSAA-JRNSEQ.                  <V76F12>
           MOVE WSAA-JRNSEQ            TO S2610-DOCSEQ.                 <V76F12>
           MOVE RCPT-ORIGAMT           TO S2610-DOCORIGAMT.             <V76F12>
           MOVE RCPT-ORIGCCY           TO S2610-ORIGCURR.               <V76F12>
           MOVE RCPT-SCRATE            TO S2610-SCRATE.                 <V76F12>
           MOVE RCPT-ACCTAMT           TO S2610-DOCACCTAMT.             <V76F12>
           MOVE WSSP-ACCTCCY           TO S2610-ACCTCCY.                <V76F12>
                                                                        <V76F12>
           IF SBM-PARTIAL-CANCEL                                        <V76F12>
           OR SBM-MODIFY                                                <V76F12>
              PERFORM 1311-LOAD-FIRST-DISSECTION                        <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           IF RCPT-PRTFLG           NOT = SPACES                        <V76F12>
              MOVE 'Y'                 TO WSAA-PRINTED                  <V76F12>
           ELSE                                                         <V76F12>
              MOVE SPACES              TO WSAA-PRINTED                  <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
       1610-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       1620-LOAD-EXISTING-RCPT SECTION.                                 <V76F12>
      *********************************                                 <V76F12>
       1620-BEGIN.                                                      <V76F12>
           PERFORM C200-RCPTIO.                                         <V76F12>
           IF RCPT-STATUZ               = ENDP                          <V76F12>
           OR RCPT-RDOCPFX          NOT = PRFX-CASH                     <V76F12>
           OR RCPT-RDOCCOY          NOT = WSSP-COMPANY                  <V76F12>
           OR RCPT-RDOCNUM          NOT = WSAA-RECEIPT                  <V76F12>
              MOVE ENDP                TO RCPT-STATUZ                   <V76F12>
              GO TO 1620-EXIT                                           <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE ZEROES                 TO S2610-SELECT-NUMBER.          <V76F12>
           MOVE RCPT-SACSCODE          TO S2610-SACSCODE.               <V76F12>
           MOVE RCPT-SACSTYP           TO S2610-SACSTYPW.               <V76F12>
           MOVE RCPT-RLDGACCT          TO S2610-ENTITY.                 <V76F12>
           MOVE RCPT-TRANDESC          TO S2610-TRANDESC.               <V76F12>
           MOVE RCPT-DISACC            TO S2610-DISACC.                 <V76F12>
           MOVE RCPT-SEQNBR            TO WSAA-JRNSEQ                   <V76F12>
           MOVE WSAA-JRNSEQ            TO S2610-TRANSEQ.                <V76F12>
           MOVE RCPT-ORIGAMT           TO S2610-ORIGAMT.                <V76F12>
           MOVE RCPT-ORIGCCY           TO S2610-BCURR.                  <V76F12>
           MOVE RCPT-SLRATE            TO S2610-SLRATE.                 <V76F12>
           MOVE RCPT-ACCTAMT           TO S2610-ACCTAMT.                <V76F12>
           MOVE RCPT-SOMEM             TO S2610-SOMEM.                  <V76F12>
           MOVE RCPT-EXTRA             TO S2610-EXTRA.                  <V76F12>
           MOVE WSSP-ACCTCCY           TO S2610-GENLCUR.                <V76F12>
                                                                        <V76F12>
           MOVE RCPT-TRANKEY           TO  S2610-GENLKEY                <V76F12>
           MOVE RCPT-GLSIGN            TO  S2610-SIGN                   <V76F12>
           MOVE RCPT-CONTOT            TO  S2610-CNTTOT                 <V76F12>
           MOVE RCPT-CHDRKEY           TO  S2610-CHDRKEY                <V76F12>
           MOVE RCPT-BRANCH            TO  S2610-BRANCH                 <V76F12>
                                                                        <V76F12>
      * Move values to hidden fields.                                   <V76F12>
           MOVE RCPT-RLDGPFX           TO WSAA-TRANPFX.                 <V76F12>
           MOVE RCPT-RLDGCOY           TO WSAA-TRANCOY.                 <V76F12>
           MOVE RCPT-RLDGPFX           TO WSAA-WTRANPFX.                <V76F12>
           MOVE RCPT-RLDGCOY           TO WSAA-WTRANCOY.                <V76F12>
           MOVE RCPT-RLDGACCT          TO WSAA-ENTITY.                  <V76F12>
           MOVE WSAA-TRANKEY           TO S2610-TRANKEY.                <V76F12>
           MOVE SCRN-SUBFILE-RRN       TO WSAA-SUBFILE-RRN.             <V76F12>
           ADD 1                       TO WSAA-KFK-RRN.                 <V76F12>
           MOVE WSAA-KFK-RRN           TO S2610-KFKSEQ.                 <V76F12>
           MOVE ZEROS                  TO S2610-CNT                     <V76F12>
                                          S2610-TRANNO.                 <V76F12>
      * If no edit routine exist on T3697 for the Subaccount Code       <V76F12>
      * being processed, protect and non-display the SELECT field.      <V76F12>
           MOVE SPACES                 TO WSKY-ITEM-ITEMITEM.           <V76F12>
           STRING WSAA-PROG                                             <V76F12>
                  S2610-SACSCODE                                        <V76F12>
           DELIMITED BY SIZE         INTO WSKY-ITEM-ITEMITEM.           <V76F12>
                                                                        <V76F12>
           PERFORM 1325-READ-T3697.                                     <V76F12>
           IF T3697-VALPROG            = SPACES                         <V76F12>
              MOVE 'Y'                 TO S2610-SELNUM-OUT (PR)         <V76F12>
              MOVE 'Y'                 TO S2610-SELNUM-OUT (ND)         <V76F12>
           ELSE                                                         <V76F12>
              MOVE 'N'                 TO S2610-SELNUM-OUT (PR)         <V76F12>
              MOVE 'N'                 TO S2610-SELNUM-OUT (ND)         <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           IF WSAA-RBNK-POSTDTEFLG      = 'Y'                           <V76F12>
           AND NOT PRINTED                                              <V76F12>
           AND SBM-MODIFY                                               <V76F12>
              NEXT SENTENCE                                             <V76F12>
           ELSE                                                         <V76F12>
              PERFORM 1326-PROTECT-FIELDS                               <V76F12>
              IF SBM-PARTIAL-CANCEL                                     <V76F12>
                 MOVE 'N'              TO S2610-ORIGAMT-OUT (PR)        <V76F12>
                                          S2610-ACCTAMT-OUT (PR)        <V76F12>
              END-IF                                                    <V76F12>
           END-IF                                                       <V76F12>
           MOVE SADD                   TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9000-S2610-IO.                                       <V76F12>
                                                                        <V76F12>
       1620-NEXTR.                                                      <V76F12>
           MOVE NEXTR                  TO RCPT-FUNCTION.                <V76F12>
                                                                        <V76F12>
       1620-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
                                                                        <V66P11>
       PRE-SCREEN-EDIT SECTION.
      *************************
      *
       PRE-START.
      *
      * If returning from an optional selection skip this section.
      *                                                                 <PHLRMS>
           MOVE O-K                    TO WSSP-EDTERROR.
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              IF WSSP-LASTPROG                = 'SR22P'                 <V71F07>
                 PERFORM A2000-RETREIVE-FROM-SR22P                      <V71F07>
              ELSE                                                      <FA4263>
                 MOVE 'N'              TO WSAA-SR22P-NEW-REC            <FA4263>
              END-IF                                                    <V71F07>
              PERFORM 1311-LOAD-FIRST-DISSECTION
              MOVE 3000                TO WSSP-SECTIONNO
              IF WSSP-LASTPROG                = 'SR289'  AND            <GP4416>
                 WSAA-SR289-INDIC             = 'N'                     <GP4416>
                 MOVE 2000             TO WSSP-SECTIONNO                <GP4416>
                 MOVE 'Y'              TO WSAA-SR22P-NEW-REC            <GP4416>
                 MOVE 'Y'              TO WSAA-SR289-INDIC              <GP4416>
                 MOVE WSSP-SUBFILE-END TO WSAA-SR289-SFL-END            <FA4882>
              END-IF                                                    <GP4416>
              GO TO PRE-EXIT
           END-IF.
      *
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
           IF SCRN-DEVICE-IND           = '*RMT'                        <V62P04>
      ***     MOVE 2                   TO WSSP-SUBFILE-END      <V62P04><WEBNQ1>
      ***                                 SCRN-SUBFILE-END      <V62P04><WEBNQ1>
              MOVE SCRN-SUBFILE-END    TO WSSP-SUBFILE-END              <WEBNQ1>
           END-IF.                                                      <V62P04>
           MOVE SCRN-SUBFILE-RRN       TO WSAA-CURRENT-RRN.             <R2-296>

           MOVE O-K                    TO WSSP-EDTERROR.
           MOVE SCRN-STATUZ            TO WSAA-SCRN-STATUZ.
      **** MOVE WSSP-SUBFILE-END       TO WSAA-SR289-SFL-END.           <FA3701>
           MOVE WSSP-CLNTKEY           TO WSAA-CLNTKEY.                 <FA3600>
                                                                        <FA3600>
      *F5=CALC
           IF  WSAA-SCRN-STATUZ        = CALC
               MOVE 'Y'                TO WSSP-EDTERROR
           END-IF.

      * For inquiry , program still need to validate
      * switching information in subfile.
      * i.e. The hidden field S2610-INDIC is passed back from
      *      P2610EDCL and P2610EDTP which is used to control
      *      switching to P8016 and PN427 respectively
           IF SBM-INQUIRY
              GO TO 2060-VALIDATE-SUBFILE
           END-IF.
                                                                        <V76F12>
      *--For modification of non-postdated receipt, validated only      <V76F12>
      *--the switching part of the subfile as this screen is protected  <V76F12>
           IF SBM-MODIFY                                                <V76F12>
           AND WSAA-RBNK-POSTDTEFLG NOT = 'Y'                           <V76F12>
              GO TO 2060-VALIDATE-SUBFILE                               <V76F12>
           END-IF.                                                      <V76F12>

      *Roll Up
           IF SBM-CREATION     AND
              WSAA-SCRN-STATUZ         = ROLU
               PERFORM 2100-LOAD-SUBFILE
               MOVE 'Y'                TO WSSP-EDTERROR
               GO TO 2090-EXIT
           END-IF.
                                                                        <V76F12>
           IF  SBM-MODIFY                                               <V76F12>
           AND WSAA-RBNK-POSTDTEFLG     = 'Y'                           <V76F12>
           AND WSAA-SCRN-STATUZ         = ROLU                          <V76F12>
               PERFORM 2100-LOAD-SUBFILE                                <V76F12>
               MOVE 'Y'                TO WSSP-EDTERROR                 <V76F12>
               GO TO 2090-EXIT                                          <V76F12>
           END-IF.                                                      <V76F12>
      *
      *                                                                 <V71F07>
       2010-VALIDATE-SCREEN.
      *
           IF WSAA-SCRN-STATUZ     NOT = O-K AND 'INSR'
              PERFORM 2200-VALIDATE-FUNCTION
              GO TO 2050-CHECK-FOR-ERRORS
           END-IF.

       2050-CHECK-FOR-ERRORS.
      *
           IF S2610-ERROR-INDICATORS   NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.
                                                                        <PA4196>
           IF WSAA-SCRN-STATUZ         = 'DELT' AND                     <PA4196>
              (SBM-CANCELLATION                 OR                      <PA4196>
               SBM-PARTIAL-CANCEL      )                                <PA4196>
               MOVE G602              TO SCRN-ERROR-CODE                <PA4196>
               MOVE 'Y'               TO WSSP-EDTERROR                  <PA4196>
           END-IF.                                                      <PA4196>
      *
       2060-VALIDATE-SUBFILE.
      *
      ***  MOVE 'N'                    TO WSAA-ONE-SELECTED.            <R2-296>
      ***  MOVE 'N'                    TO WSAA-SWITCH-ACTIVE.           <R2-296>
           MOVE 'Y'                    TO WSAA-DISPLAY-IMBAL.
      ***  MOVE ZEROES                 TO WSAA-DISS-TOTAL.              <R2-296>
           MOVE ZEROES                 TO WSAA-DISS-COUNT.
           MOVE SPACES                 TO WSAA-REVISIT-CHK-BOX.         <V66P11>

      ***  MOVE SSTRT                  TO SCRN-FUNCTION.                <R2-296>
      *                                                                 <R2-296>
      *---FOR CANCELLATION, IT IS MANDATORY TO RUN THROUGH ALL THE      <R2-296>
      *     DISSECTIONS AT LEAST ONCE. THIS WILL SETUP THE EXTRA AREA   <R2-296>
      *    IF (SBM-CANCELLATION        OR SBM-PARTIAL-CANCEL) AN<V76F12><R2-296>
           IF (SBM-CANCELLATION        OR                               <V76F12>
               SBM-PARTIAL-CANCEL      OR                               <V76F12>
               SBM-MODIFY)             AND                              <V76F12>
              WSAA-FIRST-TIME           = 'Y'                           <R2-296>
              MOVE 'N'                 TO WSAA-FIRST-TIME               <R2-296>
              MOVE SSTRT               TO WSAA-SCRN-FUNCTION            <R2-296>
                                          SCRN-FUNCTION
              PERFORM 9000-S2610-IO
              MOVE SRDN                TO WSAA-SCRN-FUNCTION
             ELSE                                                       <R2-296>
             IF WSAA-SR22P-NEW-REC      = 'Y'                           <V71F07>
                 MOVE SSTRT               TO WSAA-SCRN-FUNCTION         <V71F07>
                                             SCRN-FUNCTION              <V71F07>
                 PERFORM 9000-S2610-IO                                  <V71F07>
                 MOVE SRDN                TO WSAA-SCRN-FUNCTION         <V71F07>
             ELSE                                                       <V71F07>
      *-- check if duplicate dissection exist to be able to             <GP5152>
      *-- pass appropriate SCRN function.                               <GP5152>
              IF WSAA-FOUND-DUP     = 'N'                               <V74G02>
              AND S2610-DOCTYPE  NOT = '4'                              <V74G02>
              AND                NOT = '1'                              <CS009>
              AND                NOT = '3'                              <CS009>
              OR  (SCRN-STATUZ       = 'KANJ'                           <GP5152>
              AND S2610-DOCTYPE      = '4')                             <GP5152>
              MOVE SRNCH               TO WSAA-SCRN-FUNCTION            <R2-296>
                                          SCRN-FUNCTION
              PERFORM 9000-S2610-IO
              ELSE                                                      <GP5152>
              MOVE SSTRT               TO WSAA-SCRN-FUNCTION            <GP5152>
                                          SCRN-FUNCTION                 <GP5152>
              PERFORM 9000-S2610-IO                                     <GP5152>
              MOVE SRDN                TO WSAA-SCRN-FUNCTION            <GP5152>
              END-IF                                                    <GP5152>
           END-IF.                                                      <R2-296>
      ***  PERFORM 9000-S2610-IO.                                       <R2-296>

           MOVE  'N'               TO  WSAA-INTER-BRANCH-FLAG.          <PHLRMS>
                                                                        <PHLRMS>
           MOVE ZERO               TO WSAA-COUNT-ERR-SUB-ACC.           <PHLRMS>
           MOVE ZERO               TO WSAA-COUNT-ERR-POLICY.            <PHLRMS>
           MOVE ZERO               TO IDX.                              <PHLRMS>
           PERFORM 2600-VALIDATE-SUBFILE                                <PHLRMS>
                                       UNTIL SCRN-STATUZ = ENDP         <PHLRMS>
                                                                        <PHLRMS>
           IF  WSSP-SBMACTION          = 'A'                            <PHLRMS>
               IF  S2610-CONFIRM       NOT = 'Y'                        <PHLRMS>
               AND WSSP-SERVUNIT       NOT = 'TR'                       <PHLRMS>
                   IF  WSSP-EDTERROR   = 'Y'                            <PHLRMS>
                       MOVE 'N'        TO S2610-CONFIRM                 <PHLRMS>
                   ELSE                                                 <PHLRMS>
                       MOVE 'Y'        TO S2610-CONFIRM                 <PHLRMS>
                   END-IF                                               <PHLRMS>
                   IF  WSAA-COUNT-ERR-SUB-ACC  > 0                      <PHLRMS>
                   OR  WSAA-COUNT-ERR-POLICY   > 0                      <PHLRMS>
                       MOVE WSAA-WARN  TO S2610-SFLAG                   <PHLRMS>
                       MOVE 'N'        TO S2610-CONFIRM-OUT  (PR)       <PHLRMS>
                       MOVE 'N'        TO S2610-CONFIRM-OUT  (ND)       <PHLRMS>
                   ELSE                                                 <PHLRMS>
                       MOVE SPACES     TO S2610-SFLAG                   <PHLRMS>
                       MOVE 'Y'        TO S2610-CONFIRM-OUT  (PR)       <PHLRMS>
                       MOVE 'Y'        TO S2610-CONFIRM-OUT  (ND)       <PHLRMS>
                   END-IF                                               <PHLRMS>
                                                                        <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <GP5152>
      *    Disallow Duplication of Sub Account Code+Sub Account Type+   <GP5152>
      *    Entity                                                       <GP5152>
      *                                                                 <GP5152>
                                                                        <GP5152>
           IF T3719-IND-11              = 'Y'    AND                    <GP5152>
              T3719-IND-12              = 'Y'                           <GP5152>
           INITIALIZE                     WSAA-DP-SUBFILE-RRN           <GP5152>
                                          WSAA-DP-SACSCODE              <GP5152>
                                          WSAA-DP-SACSTYPW              <GP5152>
                                          WSAA-DP-ENTITY                <GP5152>
                                          WSAA-OUT-LOOP-STATUZ          <GP5152>
                                          WSAA-IN-LOOP-STATUZ           <GP5152>
                                                                        <GP5152>
           MOVE  SCRN-SUBFILE-RRN      TO WSAA-LAST-SUBFILE-RRN         <GP5152>
      *                                                                 <GP5152>
           MOVE SPACES                 TO SCRN-STATUZ                   <GP5152>
           MOVE 1                      TO SCRN-SUBFILE-RRN              <GP5152>
           MOVE SSTRT                  TO SCRN-FUNCTION                 <GP5152>
           CALL 'S2610IO' USING SCRN-SCREEN-PARAMS                      <GP5152>
                                          S2610-DATA-AREA               <GP5152>
                                          S2610-SUBFILE-AREA            <GP5152>
           PERFORM 5600-CHECK-DUPLICATES                                <GP5152>
              UNTIL WSAA-OUT-LOOP-STATUZ = ENDP                         <GP5152>
           END-IF.                                                      <GP5152>
      *
           MOVE 1                      TO SCRN-SUBFILE-RRN.
           PERFORM 2250-CHCK-CHECK-BOX.                                 <V66P11>

      * For inquiry mode ,skip validation below
           IF SBM-INQUIRY
           OR (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                       <V76F12>
               SBM-MODIFY)                                              <V76F12>
              PERFORM 2700-DOUBLE-CONFIRM                               <V76F12>
              MOVE WSAA-CURRENT-RRN    TO SCRN-SUBFILE-RRN              <R2-296>
              GO TO 2090-EXIT
           END-IF.

      * For partial cancellation, user must select at least one subfile
      ***  IF SBM-PARTIAL-CANCEL  AND                                   <R2-296>
      ***     NOT ONE-SELECTED                                          <R2-296>
           IF SBM-PARTIAL-CANCEL  AND                                   <R2-296>
              WSAA-NUM-SELECTED         = ZEROS                         <R2-296>
              MOVE SREAD               TO SCRN-FUNCTION
              PERFORM 9000-S2610-IO

              MOVE E207                TO S2610-SELNUM-ERR
              MOVE 'Y'                 TO WSSP-EDTERROR

              MOVE SUPD                TO SCRN-FUNCTION
              PERFORM 9000-S2610-IO
              GO                       TO 2090-EXIT                     <R2-296>
           END-IF.
           MOVE WSAA-CURRENT-RRN       TO SCRN-SUBFILE-RRN.             <R2-296>
      *                                                                 <R2-296>
      *---IF THIS IS A PARTIAL CANCELLATION, AND IF WE ARE SWITCHING OUT<R2-296>
      *      THEN WE CANNOT CHECK IMBALANCE. BECOZ SURELY IT WILL NOT   <R2-296>
      *      BALANCE AS ONLY A '2' WILL BE ADDED TO THE TRANSACTION     <R2-296>
      *      AMOUNT. NOW WE ASSUME THAT IF WSAA-NUM-SELECTED IS ODD,    <R2-296>
      *      THEN THERE IS A SWITCHING REQUEST.                         <R2-296>
           DIVIDE WSAA-NUM-SELECTED    BY 2                             <R2-296>
                  GIVING IX REMAINDER  IY.                              <R2-296>
           IF SBM-PARTIAL-CANCEL          AND                           <R2-296>
              IY                        = 1                             <R2-296>
              GO                       TO 2090-EXIT                     <R2-296>
           END-IF.                                                      <R2-296>

      * Check imbalance amount only if subfile has no errors
           IF DISPLAY-IMBAL
              PERFORM 2300A-ACCT-AMOUNT                                 <FA4960>
              PERFORM 2300-CHECK-IMBALANCE
           END-IF.
                                                                        <FA4568>
      *--If there is no imbalance error we check the GL accounts        <FA5241>
      *--if balance.                                                    <FA5241>
      *    IF  WSSP-EDTERROR            = O-K                   <FA5281><FA5241>
      *    AND WSAA-SCRN-STATUZ     NOT = 'KANJ'                <FA5281><FA5241>
      *       PERFORM 2300C-CHECK-GL-BALANCE                    <FA5281><FA5241>
      *    END-IF.                                              <FA5281><FA5241>
                                                                        <FA5241>
      * There should be atleast one dissection entry                    <FA4568>
           IF WSAA-SCRN-STATUZ       NOT = 'SERV' AND                   <FA4905>
              WSAA-SCRN-STATUZ       NOT = 'KANJ' AND                   <FA4905>
              WSAA-SCRN-STATUZ       NOT = 'DELT' AND                   <FA4905>
              WSAA-SCRN-STATUZ       NOT = 'INSR'                       <FA4905>
           AND WSSP-EDTERROR             = O-K                          <FA4915>
      *     PERFORM 2400-DISSECTION-COUNT                       <FA4905><FA4568>
      *     IF WSAA-DISSEC-COUNT          = ZERO                <FA4905><FA4568>
      *        MOVE F413                 TO SCRN-ERROR-CODE     <FA4905><FA4568>
      *        MOVE 'Y'                  TO WSSP-EDTERROR       <FA4905><FA4568>
      *     END-IF                                              <FA4905><FA4568>
              PERFORM 2400-DISSECTION-COUNT                             <FA4905>
              IF WSAA-DISSEC-COUNT          = ZERO                      <FA4905>
                 MOVE F413                 TO SCRN-ERROR-CODE           <FA4905>
                 MOVE 'Y'                  TO WSSP-EDTERROR             <FA4905>
              END-IF                                                    <FA4905>
           END-IF.                                                      <FA4905>

      * Check if ONLY 1 Dissection should be present.
           IF WSAA-DISS-COUNT             > 1         AND
              WSAA-ONE-DISS           NOT = SPACES
      * Don't populate the warning if remote invocation.                <WEBNQ1>
           AND SCRN-DEVICE-IND         NOT = '*RMT'                     <WEBNQ1>
              MOVE R012                TO SCRN-ERROR-CODE
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.
                                                                        <FA5281>
      *--If there is no imbalance error we check the GL accounts        <FA5281>
      *--if balance.                                                    <FA5281>
           IF  WSSP-EDTERROR            = O-K                           <FA5281>
           AND WSAA-SCRN-STATUZ     NOT = 'KANJ'                        <FA5281>
           AND WSAA-SCRN-STATUZ     NOT = 'DELT'                        <FA5281>
           AND WSAA-SCRN-STATUZ     NOT = 'SERV'                        <FA5281>
              PERFORM 2300C-CHECK-GL-BALANCE                            <FA5281>
           END-IF.                                                      <FA5281>
                                                                        <V6F114>
           IF  INTER-BRANCH-YES    AND                                  <V6F114>
               SCRN-DEVICE-IND    NOT = '*RMT'     AND                  <FA3062>
               S2610-ERROR-SUBFILE    =  SPACES    AND                  <V6F114>
               S2610-ERROR-INDICATORS =  SPACES                         <V6F114>
               MOVE SPACES             TO MBOX-CPFMSG MBOX-INSERT       <V6F114>
                                          MBOX-REPLY  MBOX-RESULT       <V6F114>
               MOVE WSSP-LANGUAGE      TO WSAA-LANGUAGE                 <V6F114>
               MOVE '0001'             TO WSAA-MSGID                    <V6F114>
               MOVE WSAA-PAXMSG        TO MBOX-CPFMSG                   <V6F114>
               PERFORM 2500-DISPLAY-MSGBOX                              <V6F114>
               MOVE 'N'                TO WSAA-INTER-BRANCH-FLAG        <R2-292>
               MOVE 'Y'                TO WSAA-FIRST-WARNING            <R2-292>
           END-IF.                                                      <V6F114>

      *--Check if double confirmation is require to complete the        <V76F02>
      *--transaction.                                                   <V76F02>
           PERFORM 2700-DOUBLE-CONFIRM.                                 <V76F02>
                                                                        <PHLRMS>
                                                                        <V76F02>
       2090-EXIT.
            EXIT.
      /
       2100-LOAD-SUBFILE SECTION.
      ***************************
       2100-BEGIN.
           MOVE 1                      TO WSAA-SFL-CNT.

           PERFORM 1312-LOAD-BLANK-SUBFILE UNTIL
                          WSAA-SFL-CNT > S2610-SUBFILE-PAGE.

           MOVE 'Y'                   TO SCRN-SUBFILE-MORE.
      *
       2100-EXIT.
            EXIT.
      /
       2200-VALIDATE-FUNCTION SECTION.
      ********************************
       2200-BEGIN.

           MOVE 'CHCK'                 TO OPTS-FUNCTION.
           MOVE WSAA-PROG              TO OPTS-CALLING-PROG.
           MOVE ZEROES                 TO OPTS-DTEEFF.
           MOVE WSSP-COMPANY           TO OPTS-COMPANY.                 <V76F12>
           MOVE WSSP-LANGUAGE          TO OPTS-LANGUAGE.
           MOVE WSSP-TRANID            TO VRCM-TRANID.
           MOVE VRCM-USER              TO OPTS-USER.
           MOVE ZEROES                 TO OPTS-SEL-OPTNO.
           MOVE 'F'                    TO OPTS-SEL-TYPE.
           MOVE SCRN-STATUZ            TO OPTS-SEL-CODE.

           CALL 'OPTSWCH'        USING OPTSWCH-REC
                                       WSSP-SEC-PROGS
                                       WSSP-SEC-ACTNS
                                       WSSP-PROGRAM-PTR
                                       WSSP-FLAG.

           IF OPTS-STATUZ           NOT = O-K
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.

       2200-EXIT.
           EXIT.
      /
       2250-CHCK-CHECK-BOX SECTION.                                     <V66P11>
      *****************************                                     <V66P11>
       2251-START.                                                      <V66P11>
           IF  ENTER-CN-SCREEN                                          <V66P11>
           AND COVER-NOTE-ALLOWED                                       <V66P11>
               IF  SVCB-IND-UPDATE      = SPACES                        <V66P11>
                   MOVE 'X'            TO S2610-XOPTIND                 <V66P11>
               END-IF                                                   <V66P11>
           END-IF.                                                      <V66P11>
                                                                        <V66P11>
           IF  S2610-XOPTIND            = SPACES                        <V66P11>
               GO TO 2259-EXIT                                          <V66P11>
           END-IF.                                                      <V66P11>
                                                                        <V66P11>
           IF  NOT SBM-INQUIRY                                          <V66P11>
           AND NOT ENTER-CN-SCREEN                                      <V66P11>
           AND NOT COVER-NOTE-ALLOWED                                   <V66P11>
           AND NOT (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                  <V76F12>
                    SBM-MODIFY)                                         <V76F12>
               MOVE F612               TO S2610-XOPTIND-ERR             <V66P11>
               MOVE 'Y'                TO WSSP-EDTERROR                 <V66P11>
               GO TO 2259-EXIT                                          <V66P11>
           END-IF.                                                      <V66P11>
                                                                        <V66P11>
           IF  S2610-XOPTIND            = '+'                           <V66P11>
           AND NOT SBM-INQUIRY                                          <V66P11>
           AND REVISIT-CHK-BOX                                          <V66P11>
           AND COVER-NOTE-ALLOWED                                       <V66P11>
           AND NOT (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                  <V76F12>
                    SBM-MODIFY)                                         <V76F12>
               MOVE 'X'                TO S2610-XOPTIND                 <V66P11>
               GO TO 2259-EXIT                                          <V66P11>
           END-IF.                                                      <V66P11>
                                                                        <V66P11>
           MOVE 'CHCK'                 TO OPTS-FUNCTION.                <V66P11>
           MOVE WSAA-PROG              TO OPTS-CALLING-PROG.            <V66P11>
           MOVE WSAA-TODAY             TO OPTS-DTEEFF.                  <V66P11>
           MOVE WSSP-COMPANY           TO OPTS-COMPANY.                 <V66P11>
           MOVE WSSP-LANGUAGE          TO OPTS-LANGUAGE.                <V66P11>
           MOVE WSSP-TRANID            TO VRCM-TRANID.                  <V66P11>
           MOVE VRCM-USER              TO OPTS-USER.                    <V66P11>
           MOVE 'CBOX'                 TO OPTS-SEL-CODE.                <V66P11>
                                                                        <V66P11>
           PERFORM VARYING WSAA-COUNT FROM 1 BY 1                       <V66P11>
             UNTIL WSAA-COUNT > WSAA-MAX-OPT                            <V66P11>
               IF  OPTS-TYPE (WSAA-COUNT) = 'X'                         <V66P11>
                   MOVE S2610-XOPTIND  TO OPTS-IND (WSAA-COUNT)         <V66P11>
                                                                        <V66P11>
                   CALL 'OPTSWCH'   USING OPTSWCH-REC                   <V66P11>
                                          WSSP-SEC-PROGS                <V66P11>
                                          WSSP-SEC-ACTNS                <V66P11>
                                          WSSP-PROGRAM-PTR              <V66P11>
                                          WSSP-FLAG                     <V66P11>
                                                                        <V66P11>
                   IF OPTS-STATUZ   NOT = O-K                           <V66P11>
                      MOVE OPTS-STATUZ TO S2610-XOPTIND-ERR             <V66P11>
                      MOVE 'Y'         TO WSSP-EDTERROR                 <V66P11>
                   END-IF                                               <V66P11>
               END-IF                                                   <V66P11>
           END-PERFORM.                                                 <V66P11>
                                                                        <V66P11>
       2259-EXIT.                                                       <V66P11>
           EXIT.                                                        <V66P11>
                                                                        <V66P11>
       2300-CHECK-IMBALANCE SECTION.
      ******************************
       2300-BEGIN.
      *
      * After subfile validation, update the Imbalance total (in
      * base-currency). Note: Manual adjustment of decimal digits on
      * the screen may be necessary because of rounded results.
      *
           MOVE ZEROS                  TO WSAA-GLIMBAL.                 <FA4960>
           COMPUTE S2610-IMBALAM = S2610-DOCACCTAMT - WSAA-DISS-TOTAL.
           COMPUTE WSAA-GLIMBAL  = S2610-DOCACCTAMT - WSAA-GLAMT-TOTAL. <FA4960>
      *
           MOVE 'Y'                    TO S2610-IMBALAM-OUT (ND).
      *
           IF WSAA-SCRN-STATUZ     NOT = 'SERV' AND
              WSAA-SCRN-STATUZ     NOT = 'KANJ'
           AND WSAA-SCRN-STATUZ     NOT = 'DELT'                        <V71F07>

              PERFORM 2310-CHECK-FOREIGN-CURRENCY
              IF TRNC-STATUZ           NOT = BOMB AND O-K
                 MOVE 'N'                 TO S2610-IMBALAM-OUT (ND)
                 MOVE TRNC-STATUZ         TO S2610-IMBALAM-ERR
                 MOVE TRNC-ORIGCURR       TO S2610-IMBALCCY
                 MOVE TRNC-IMBALANCE      TO S2610-IMBALAM
                 MOVE 'Y'                 TO WSSP-EDTERROR
              ELSE
                 IF S2610-IMBALAM         NOT = ZEROES
                    PERFORM 2330-CHECK-ROUNDING                         <FA4031>
      ***           MOVE 'N'              TO S2610-IMBALAM-OUT (ND)     <FA4031>
      ***           MOVE E198             TO S2610-IMBALAM-ERR          <FA4031>
      ***           MOVE 'Y'              TO WSSP-EDTERROR              <FA4031>
                 END-IF
              END-IF
           END-IF.
       2300-EXIT.
           EXIT.
      /
       2310-CHECK-FOREIGN-CURRENCY SECTION.
      *************************************
       2310-BEGIN.
      *
      * Once the base-currency receipt balances, pass the header
      * details to the subroutine, then check whether the
      * foreign-currency receipt balances as well.
      * Subtract what the header amount used to be first, then store
      * the current one and add it.
      *
           MOVE 'SUBTR'                TO TRNC-FUNCTION.
           MOVE WSSP-SIGN              TO TRNC-GLSIGN.
           MOVE S2610-ORIGCURR         TO TRNC-ORIGCURR.
           MOVE WSAA-STORE-DOCORIGAMT  TO TRNC-ORIGAMT.

           CALL 'TRNCURBAL'         USING TRNC-TRNCURBAL-REC.

           IF TRNC-STATUZ           NOT = O-K
              MOVE 'TRNCURBAL'         TO SYSR-SUBRNAME
              MOVE TRNC-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
           MOVE S2610-DOCORIGAMT       TO WSAA-STORE-DOCORIGAMT.

           PERFORM 1190-INIT-TRNCURBAL.                                 <FA3943>
      *                                                                 <FA3943>
           MOVE 'ADD  '                TO TRNC-FUNCTION.
           MOVE WSSP-SIGN              TO TRNC-GLSIGN.
           MOVE S2610-ORIGCURR         TO TRNC-ORIGCURR.
           MOVE S2610-DOCORIGAMT       TO TRNC-ORIGAMT.

           CALL 'TRNCURBAL' USING TRNC-TRNCURBAL-REC.

           IF TRNC-STATUZ              NOT =  O-K
              MOVE 'TRNCURBAL'         TO SYSR-SUBRNAME
              MOVE TRNC-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
           MOVE SSTRT                  TO SCRN-FUNCTION.                <FA3943>
           PERFORM 9000-S2610-IO.                                       <FA3943>
           PERFORM 2320-CHECK-ORIG-DISS UNTIL SCRN-STATUZ = ENDP.       <FA3943>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <FA3943>
      *                                                                 <FA3943>
           MOVE 'CHECK'                TO TRNC-FUNCTION.

           CALL 'TRNCURBAL'         USING TRNC-TRNCURBAL-REC.

           IF TRNC-STATUZ              = BOMB
              MOVE 'TRNCURBAL'         TO SYSR-SUBRNAME
              MOVE TRNC-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

       2310-EXIT.
           EXIT.
      *                                                                 <FA3943>
       2320-CHECK-ORIG-DISS SECTION.                                    <FA3943>
      ******************************                                    <FA3943>
       2320-START.                                                      <FA3943>
      *                                                                 <FA3943>
      *----Only accumulated the amount for the selected line            <FA3943>
           IF  SBM-PARTIAL-CANCEL                                       <FA3943>
           AND S2610-SELECT-NUMBER      = ZEROS                         <FA3943>
             GO TO 2320-NEXT                                            <FA3943>
           END-IF.                                                      <FA3943>
                                                                        <FA3943>
           MOVE S2610-BCURR            TO S2610-ORIGCUR.                <FA3943>
           MOVE S2610-ORIGAMT          TO S2610-AMTRI.                  <FA3943>
           MOVE S2610-GLSIGN           TO S2610-SIGN.                   <FA3943>
      *                                                                 <FA3943>
           MOVE 'ADD  '                TO TRNC-FUNCTION.                <FA3943>
           MOVE S2610-ORIGAMT          TO TRNC-ORIGAMT.                 <FA3943>
           MOVE S2610-BCURR            TO TRNC-ORIGCURR.                <FA3943>
           MOVE S2610-GLSIGN           TO TRNC-GLSIGN.                  <FA3943>
                                                                        <FA3943>
           CALL 'TRNCURBAL'         USING TRNC-TRNCURBAL-REC.           <FA3943>
                                                                        <FA3943>
           IF TRNC-STATUZ           NOT = O-K                           <FA3943>
              MOVE 'TRNCURBAL'         TO SYSR-SUBRNAME                 <FA3943>
              MOVE TRNC-STATUZ         TO SYSR-STATUZ                   <FA3943>
              PERFORM 600-FATAL-ERROR                                   <FA3943>
           END-IF.                                                      <FA3943>
                                                                        <FA3943>
       2320-NEXT.                                                       <FA3943>
           MOVE SRDN                TO SCRN-FUNCTION.                   <FA3943>
           PERFORM 9000-S2610-IO.                                       <FA3943>
      *                                                                 <FA3943>
       2320-EXIT.                                                       <FA3943>
           EXIT.                                                        <FA3943>
      *                                                                 <FA3943>
                                                                        <FA4031>
       2330-CHECK-ROUNDING SECTION.                                     <FA4031>
      *****************************                                     <FA4031>
       2331-BEGIN.                                                      <FA4031>
      *                                                                 <V76F06>
                                                                        <PHLRMS>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <V76F06>
           MOVE '0'                    TO ITEM-ITEMCOY.                 <V76F06>
           MOVE T1667                  TO ITEM-ITEMTABL.                <V76F06>
           MOVE S2610-ACCTCCY          TO ITEM-ITEMITEM.                <V76F06>
      *                                                                 <V76F06>
           MOVE READR                  TO ITEM-FUNCTION.                <V76F06>
      *                                                                 <V76F06>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <V76F06>
      *                                                                 <V76F06>
           IF ITEM-STATUZ          NOT = O-K AND MRNF                   <V76F06>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V76F06>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V76F06>
              PERFORM 600-FATAL-ERROR                                   <V76F06>
           END-IF.                                                      <V76F06>
      *                                                                 <V76F06>
           IF ITEM-STATUZ              = MRNF                           <V76F06>
              MOVE RFIL                TO S2610-ACCTCCY-ERR             <V76F06>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <V76F06>
              GO TO 2339-EXIT                                           <V76F06>
           END-IF.                                                      <V76F06>
      *                                                                 <V76F06>
           MOVE ITEM-GENAREA           TO T1667-T1667-REC.              <V76F06>
      *                                                                 <V76F06>
           IF T1667-FIELD-DECIMAL-PLACES                                <V76F06>
                                       = 2 OR 3                         <V76F06>
              MOVE 0.01                TO WSAA-IMBAL                    <V76F06>
              MOVE -0.01               TO WSAA-IMBAL-N                  <V76F06>
              GO TO 2333-IMBAL-CHECKING                                 <V76F06>
           ELSE                                                         <V76F06>
              IF T1667-FIELD-DECIMAL-PLACES                             <V76F06>
                                       = 1                              <V76F06>
              MOVE 0.10                TO WSAA-IMBAL                    <V76F06>
              MOVE -0.10               TO WSAA-IMBAL-N                  <V76F06>
              GO TO 2333-IMBAL-CHECKING                                 <V76F06>
           END-IF.                                                      <V76F06>
      *                                                                 <V76F06>
           IF T1667-INTEGER        NOT = 0                              <V76F06>
              COMPUTE WSAA-IMBAL       = 1 * (10 **(T1667-INTEGER - 1)) <V76F06>
              COMPUTE WSAA-IMBAL-N     = 0 - WSAA-IMBAL                 <V76F06>
           END-IF.                                                      <V76F06>
      *                                                                 <V76F06>
       2333-IMBAL-CHECKING.                                             <V76F06>
                                                                        <FA4031>
      *    IF S2610-IMBALAM          = 0.01 OR -0.01            <FA4031><V76F06>
           IF S2610-IMBALAM          = WSAA-IMBAL OR WSAA-IMBAL-N       <V76F06>
      *                                                                 <FA4960>
      * Check if imbalance is caused by the acounting currency only     <FA4960>
      * dissections else it should be the foriegn currency.             <FA4960>
      *       IF WSAA-GLIMBAL        = 0.01 OR -0.01            <FA4960><V76F06>
              IF WSAA-GLIMBAL        = WSAA-IMBAL OR WSAA-IMBAL-N       <V76F06>
                 MOVE 'N'           TO S2610-IMBALAM-OUT (ND)           <FA4960>
                 MOVE F465          TO S2610-IMBALAM-ERR                <FA4960>
                 MOVE 'Y'           TO WSSP-EDTERROR                    <FA4960>
                 GO TO 2339-EXIT                                        <FA4960>
              END-IF                                                    <FA4960>
      *                                                                 <V76F06>
                                                                        <FA4960>
              MOVE 1                TO SCRN-SUBFILE-RRN                 <FA4031>
              MOVE SREAD            TO SCRN-FUNCTION                    <FA4031>
              CALL 'S2610IO'     USING SCRN-SCREEN-PARAMS               <FA4031>
                                       S2610-DATA-AREA                  <FA4031>
                                       S2610-SUBFILE-AREA               <FA4031>
              IF SCRN-STATUZ     NOT = O-K                              <FA4031>
                 MOVE SCRN-STATUZ   TO SYSR-STATUZ                      <FA4031>
                 PERFORM 600-FATAL-ERROR                                <FA4031>
              END-IF                                                    <FA4031>
                                                                        <FA4031>
              IF S2610-BCURR     NOT = S2610-GENLCUR                    <FA4031>
                 ADD S2610-IMBALAM  TO S2610-ACCTAMT                    <FA4031>
                                        WSAA-DISS-TOTAL                 <FA4031>
                 MOVE S2610-ACCTAMT TO S2610-AMNT                       <FA4031>
                 MOVE SUPD          TO SCRN-FUNCTION                    <FA4031>
                 CALL 'S2610IO'  USING SCRN-SCREEN-PARAMS               <FA4031>
                                       S2610-DATA-AREA                  <FA4031>
                                       S2610-SUBFILE-AREA               <FA4031>
                 IF SCRN-STATUZ  NOT = O-K                              <FA4031>
                    MOVE SCRN-STATUZ TO SYSR-STATUZ                     <FA4031>
                    PERFORM 600-FATAL-ERROR                             <FA4031>
                 END-IF                                                 <FA4031>
                 MOVE ZEROS         TO S2610-ACCTAMT                    <FA4031>
               ELSE                                                     <FA4031>
                 MOVE 'N'           TO S2610-IMBALAM-OUT (ND)           <FA4031>
                 MOVE E198          TO S2610-IMBALAM-ERR                <FA4031>
                 MOVE 'Y'           TO WSSP-EDTERROR                    <FA4031>
               END-IF                                                   <FA4031>
            ELSE                                                        <FA4031>
             MOVE 'N'              TO S2610-IMBALAM-OUT (ND)            <FA4031>
             MOVE E198             TO S2610-IMBALAM-ERR                 <FA4031>
             MOVE 'Y'              TO WSSP-EDTERROR                     <FA4031>
           END-IF.                                                      <FA4031>
                                                                        <FA4031>
       2339-EXIT.                                                       <FA4031>
           EXIT.                                                        <FA4031>
      *                                                                 <FA4960>
       2300A-ACCT-AMOUNT SECTION.                                       <FA4960>
      **************************                                        <FA4960>
       2300A-START.                                                     <FA4960>
      *                                                                 <FA4960>
      * Read the subfile to accumulate all the accounting currency      <FA4960>
      * entries.                                                        <FA4960>
           MOVE ZEROS                  TO WSAA-GLAMT-TOTAL              <FA4960>
                                          WSAA-GLIMBAL.                 <FA4960>
           MOVE SPACES                 TO SCRN-STATUZ.                  <FA4960>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <FA4960>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <FA4960>
                                                                        <FA4960>
           PERFORM 9000-S2610-IO.                                       <FA4960>
           PERFORM 2300B-CHECK-DISSECTIONS                              <FA4960>
                      UNTIL SCRN-STATUZ = ENDP.                         <FA4960>
      *                                                                 <FA4960>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <FA4960>
                                                                        <FA4960>
       2300A-EXIT.                                                      <FA4960>
           EXIT.                                                        <FA4960>
      *                                                                 <FA4960>
       2300B-CHECK-DISSECTIONS SECTION.                                 <FA4960>
      *******************************                                   <FA4960>
       2300B-START.                                                     <FA4960>
           IF S2610-BCURR               = S2610-GENLCUR                 <FA4960>
              ADD S2610-ACCTAMT        TO WSAA-GLAMT-TOTAL              <FA4960>
           END-IF.                                                      <FA4960>
                                                                        <FA4960>
           MOVE SRDN                TO SCRN-FUNCTION.                   <FA4960>
           PERFORM 9000-S2610-IO.                                       <FA4960>
                                                                        <FA4960>
       2300B-EXIT.                                                      <FA4960>
           EXIT.                                                        <FA4960>
      /                                                                 <FA5241>
       2300C-CHECK-GL-BALANCE SECTION.                                  <FA5241>
      ********************************                                  <FA5241>
       2300C-CHECK-GL.                                                  <FA5241>
      *--Initialize Array                                               <FA5241>
           PERFORM VARYING IX FROM 1 BY 1                               <FA5241>
              UNTIL        IX > WSAA-MAX-GL-ENT                         <FA5241>
              MOVE SPACES              TO WSAA-GL-BCURR   (IX)          <FA5241>
              MOVE ZEROES              TO WSAA-GL-ORIGAMT (IX)          <FA5241>
           END-PERFORM.                                                 <FA5241>
                                                                        <FA5241>
      *--Update 1st arrary record from Payment Header Document amounts  <FA5241>
           MOVE S2610-ORIGCURR         TO WSAA-GL-BCURR(01).            <FA5241>
           IF WSSP-SIGN                 = '-'                           <FA5241>
              COMPUTE WSAA-GL-ORIGAMT(01) =                             <FA5241>
                                          S2610-DOCORIGAMT * -1         <FA5241>
           ELSE                                                         <FA5241>
              MOVE S2610-DOCORIGAMT    TO WSAA-GL-ORIGAMT(01)           <FA5241>
           END-IF.                                                      <FA5241>
                                                                        <FA5241>
      *--At this point we accumulate the GL entries from the subfile.   <FA5241>
           MOVE SPACES                 TO SCRN-STATUZ.                  <FA5241>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <FA5241>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <FA5241>
           PERFORM 9000-S2610-IO.                                       <FA5241>
                                                                        <FA5241>
           PERFORM 2300D-ACCUMULATE-GL UNTIL SCRN-STATUZ = ENDP.        <FA5241>
                                                                        <FA5241>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <FA5241>
                                                                        <FA5241>
      *--Check GL imbalance error                                       <FA5241>
           PERFORM VARYING IX FROM      1 BY 1                          <FA5241>
                   UNTIL IX             > WSAA-MAX-GL-ENT OR            <FA5241>
                   WSSP-EDTERROR    NOT = O-K             OR            <FA5241>
                   WSAA-GL-BCURR(IX)    = SPACES                        <FA5241>
             IF WSAA-GL-ORIGAMT(IX) NOT = ZEROES                        <FA5241>
                MOVE RFKE                TO S2610-IMBALAM-ERR           <FA5241>
                MOVE 'Y'                 TO WSSP-EDTERROR               <FA5241>
             END-IF                                                     <FA5241>
           END-PERFORM.                                                 <FA5241>
       2300C-EXIT-GL-CHK.                                               <FA5241>
           EXIT.                                                        <FA5241>
      /                                                                 <FA5241>
       2300D-ACCUMULATE-GL SECTION.                                     <FA5241>
      *****************************                                     <FA5241>
       2300D-ACCUMULATE.                                                <FA5241>
           IF SBM-PARTIAL-CANCEL                                        <FA5381>
           AND S2610-SELECT-NUMBER  NOT = 2                             <FA5381>
               GO TO 2300D-NEXT-SFL                                     <FA5381>
           END-IF.                                                      <FA5381>
                                                                        <FA5381>
           IF S2610-BCURR           NOT = SPACES                        <FA5241>
              PERFORM VARYING IX FROM   1 BY 1                          <FA5241>
                      UNTIL IX          > WSAA-MAX-GL-ENT OR            <FA5241>
                      WSAA-GL-BCURR(IX) = S2610-BCURR     OR            <FA5241>
                      WSAA-GL-BCURR(IX) = SPACES                        <FA5241>
                 CONTINUE                                               <FA5241>
              END-PERFORM                                               <FA5241>
              MOVE S2610-BCURR         TO WSAA-GL-BCURR(IX)             <FA5241>
              IF S2610-GLSIGN           = '-'                           <FA5241>
                 COMPUTE WSAA-GL-ORIGAMT(IX)                            <FA5241>
                                        = WSAA-GL-ORIGAMT(IX) +         <FA5241>
                                         (S2610-ORIGAMT * -1)           <FA5241>
              ELSE                                                      <FA5241>
                 ADD S2610-ORIGAMT     TO WSAA-GL-ORIGAMT(IX)           <FA5241>
              END-IF                                                    <FA5241>
           END-IF.                                                      <FA5241>
                                                                        <FA5241>
       2300D-NEXT-SFL.                                                  <FA5381>
           MOVE SRDN                TO SCRN-FUNCTION.                   <FA5241>
           PERFORM 9000-S2610-IO.                                       <FA5241>
       2300D-ACCUMULATE-EXIT.                                           <FA5241>
           EXIT.                                                        <FA5241>
      *                                                                 <FA4960>
       2400-DISSECTION-COUNT SECTION.                                   <FA4568>
      *******************************                                   <FA4568>
       2400-START.                                                      <FA4568>
           MOVE ZEROES                 TO WSAA-DISSEC-COUNT             <FA4568>
           MOVE SPACES                 TO SCRN-STATUZ.                  <FA4568>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <FA4568>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <FA4568>
                                                                        <FA4568>
           PERFORM 9000-S2610-IO.                                       <FA4568>
           PERFORM 2420-CHECK-DISSECTIONS                               <FA4568>
                      UNTIL SCRN-STATUZ = ENDP.                         <FA4568>
      *                                                                 <FA4568>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <FA4568>
                                                                        <FA4568>
       2400-EXIT.                                                       <FA4568>
           EXIT.                                                        <FA4568>
      *                                                                 <FA4568>
       2420-CHECK-DISSECTIONS SECTION.                                  <FA4568>
      *******************************                                   <FA4568>
       2420-START.                                                      <FA4568>
           IF S2610-SACSCODE     NOT = SPACES AND                       <FA4568>
              S2610-SACSTYPW     NOT = SPACES                           <FA4568>
              ADD 1                 TO WSAA-DISSEC-COUNT                <FA4568>
           END-IF.                                                      <FA4568>
                                                                        <FA4568>
           MOVE SRDN                TO SCRN-FUNCTION.                   <FA4568>
           PERFORM 9000-S2610-IO.                                       <FA4568>
                                                                        <FA4568>
       2420-EXIT.                                                       <FA4568>
           EXIT.                                                        <FA4568>
      *                                                                 <V6F114>
       2500-DISPLAY-MSGBOX SECTION.                                     <V6F114>
      *****************************                                     <V6F114>
       2500-START.                                                      <V6F114>
              CALL 'MSGBOX'         USING MBOX-CPFMSG MBOX-INSERT       <V6F114>
                                          MBOX-REPLY  MBOX-RESULT.      <V6F114>
                                                                        <V6F114>
                 MOVE 'Y'              TO WSSP-EDTERROR.                <V6F114>
       2500-EXIT.                                                       <V6F114>
           EXIT.                                                        <V6F114>
      *                                                                 <V6F114>
      /
       2600-VALIDATE-SUBFILE SECTION.
      *******************************
      *
       2610-VALIDATION.
           IF  WSSP-SBMACTION          = 'A'                            <PHLRMS>
               ADD 1                   TO IDX                           <PHLRMS>
               IF  S2610-SACSCODE      NOT = WSAA-ARR-SACSCODE (IDX)    <PHLRMS>
               OR  S2610-SACSTYPW      NOT = WSAA-ARR-SACSTYPW (IDX)    <PHLRMS>
                   MOVE SPACES         TO S2610-SFLAG                   <PHLRMS>
                   MOVE 'N'            TO S2610-CONFIRM                 <PHLRMS>
                   MOVE 'Y'            TO S2610-CONFIRM-OUT    (ND)     <PHLRMS>
                   MOVE 'Y'            TO S2610-CONFIRM-OUT    (PR)     <PHLRMS>
                   MOVE S2610-SACSCODE TO WSAA-ARR-SACSCODE (IDX)       <PHLRMS>
                   MOVE S2610-SACSTYPW TO WSAA-ARR-SACSTYPW (IDX)       <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
      * In Enquiry, only the Select field can be entered (validated
      * in the edit routine).
           IF SBM-INQUIRY         OR
              SBM-CANCELLATION    OR
              SBM-PARTIAL-CANCEL
           OR (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                       <V76F12>
               SBM-MODIFY)                                              <V76F12>
              GO TO 2616-EDIT-ROUTINE
           END-IF.
      *
       2612-DISSECTION.
                                                                        <PHLRMS>
                                                                        <PHLRMS>
      * Don't do any validation if these 4 mandatory fields are not
      * entered (i.e blank line)
           IF S2610-SACSCODE           = SPACES AND
              S2610-SACSTYPW           = SPACES AND
              S2610-ORIGAMT            = ZEROES AND
              S2610-ACCTAMT            = ZEROES
              COMPUTE WSAA-DIFF        = S2610-ACCTAMT - S2610-AMNT     <R2-296>
              MOVE S2610-ACCTAMT       TO S2610-AMNT                    <R2-296>
              ADD WSAA-DIFF            TO WSAA-DISS-TOTAL               <R2-296>

     ***      IF SCRN-SUBFILE-RRN      = 1                              <V71F07>
     ***         MOVE H933             TO S2610-SACSCODE-ERR            <V71F07>
     ***                                  S2610-SACSTYPW-ERR            <V71F07>
     ***                                  S2610-ORIGAMT-ERR             <V71F07>
     ***                                  S2610-ACCTAMT-ERR             <V71F07>
     ***      ELSE                                                      <V71F07>
      *          GO TO 2680-READ-NEXT-MODIFIED-RECORD                   <FA1640>
                 GO TO 2675-UPDATE-SUBFILE                              <FA1640>
     ***      END-IF                                                    <V71F07>
           END-IF.
                                                                        <PHLRMS>
           IF  WSSP-SBMACTION          = 'A'                            <PHLRMS>
           OR  WSSP-SBMACTION          = 'B'                            <PS008>
               IF  SCRN-SUBFILE-RRN    = 1                              <PHLRMS>
                   IF S2610-SACSCODE  NOT = WSAA-SC                     <PHLRMS>
                   OR S2610-SACSTYPW  NOT = WSAA-I                      <PHLRMS>
                      MOVE S2610-ENTITY                                 <PHLRMS>
                                      TO WSAA-POLNO                     <PHLRMS>
                   ELSE                                                 <PHLRMS>
                      PERFORM A2900-GET-POLNO                           <PHLRMS>
                   END-IF                                               <PHLRMS>
               END-IF                                                   <PHLRMS>
               IF  WSSP-SERVUNIT       = 'RN'                           <PHLRMS>
                   PERFORM 2900-CHECK-SUB-ACC                           <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      * Check available FLSH balance for journal                        <V74G02>
             IF  S2610-ORIGAMT    < ZERO                                <V74G02>
             AND S2610-DOCTYPE    = '4'                                 <V74G02>
                 PERFORM A200-CALL-SUBACCBAL                            <V74G02>
             END-IF.                                                    <V74G02>
      *
      * Subaccount Code and Type are mandatory fields.
      * (Once entered, the IO will validate the Code against T3616
      * and the Type against T3695).
      *
           IF S2610-SACSCODE           = SPACES
              MOVE E186                TO S2610-SACSCODE-ERR
           END-IF.
      *
           IF S2610-SACSTYPW           = SPACES
              MOVE E186                TO S2610-SACSTYPW-ERR
           END-IF.
                                                                        <V65F04>
           IF S2610-SACSCODE            = 'WH'                          <V65F04>
              IF SCRN-SUBFILE-RRN       = 1                             <V65F04>
                 MOVE RF64             TO S2610-SACSCODE-ERR            <V65F04>
              ELSE                                                      <V65F04>
                 PERFORM 5500-CHECK-PRIOR-SFL-REC                       <V65F04>
                 IF WSAA-DUPL-WH-SACSCODE = 'Y'                         <V65F04>
                    MOVE RF64          TO S2610-SACSCODE-ERR            <V65F04>
                 END-IF                                                 <V65F04>
              END-IF                                                    <V65F04>
           END-IF.                                                      <V65F04>
      *
      *  If TPA processing = 'Y' (T3711), do not allow
      *  'RI A' dissections. This is to avoid inconsistencies
      *  between the TPA Debtors and RI GL Postings.
      *  Note that the TPA Debtors will not post the 'RI A'
      *  dissections whereas GL Post will.
      *
           IF S2610-SACSCODE           = SUBA-RIPPAY-CODE AND
              S2610-SACSTYPW           = SUBA-RIPPAY-TYPE AND
              T3711-TPAFLG             = 'Y'
                 MOVE G317             TO S2610-SACSCODE-ERR
           END-IF.

       2614-ENTITY.
      *
      * Check with T3719 if Entity is mandatory based the Subaccount
      * Code & Type, and if the dissection must be the first record.
      *
           PERFORM 2610-CHECK-T3719-RULES.
           IF S2610-ENTITY-ERR      = E436
              GO TO 2616-EDIT-ROUTINE
           END-IF.

           IF S2610-ENTITY             = SPACES
           AND (T3719-IND-03           = SPACES AND                     <TPFIX>
               T3719-IND-04            = SPACES)                        <TPFIX>
              MOVE E186                TO S2610-ENTITY-ERR
           END-IF.
      *
      * The Entity field may contain a delimited Company number.
      *
           UNSTRING S2610-ENTITY
                    DELIMITED BY '/' OR ' '
               INTO WSAA-ENTITY-1
                    WSAA-ENTITY-2
                    WSAA-ENTITY-3.

           IF WSAA-ENTITY-3            NOT = SPACES
              MOVE F949                TO S2610-ENTITY-ERR
           END-IF.

           IF WSAA-ENTITY-2            = SPACES
              MOVE S2610-ENTITY        TO WSAA-ENTITY
           END-IF.

           IF WSAA-ENTITY-2            NOT = SPACES
              MOVE WSAA-ENTITY-2       TO WSAA-ENTITY
              MOVE WSAA-ENTITY-1       TO WSAA-TRANCOY
              PERFORM 2620-VALIDATE-COMPANY
           END-IF.
      *
           IF S2610-SACSCODE-ERR       NOT = SPACES OR
              S2610-SACSTYPW-ERR       NOT = SPACES OR
              S2610-ENTITY-ERR         NOT = SPACES
              GO TO 2620-CURRENCY-AND-RATE
           END-IF.
      *
           MOVE WSAA-TRANKEY           TO S2610-TRANKEY.
      *
       2616-EDIT-ROUTINE.
           IF  (S2610-SACSCODE          = WSAA-SACSCODE-FG              <V66P11>
           AND  S2610-SACSTYPW          = WSAA-SACSTYPE-PP)             <V66P11>
           OR  (S2610-SACSCODE          = WSAA-SACSCODE-AG              <V66P11>
           AND  S2610-SACSTYPW          = WSAA-SACSTYPE-A )             <V66P11>
                IF  SBM-PARTIAL-CANCEL                                  <V66P11>
                    IF  S2610-SELECT-NUMBER = 2                         <V66P11>
                        MOVE 'Y'       TO WSAA-COVER-NOTE-ALLOWED       <V66P11>
                    END-IF                                              <V66P11>
                ELSE                                                    <V66P11>
                    MOVE 'Y'           TO WSAA-COVER-NOTE-ALLOWED       <V66P11>
                END-IF                                                  <V66P11>
           END-IF.                                                      <V66P11>
                                                                        <V66P11>
           IF  WSAA-SCRN-FUNCTION       = SRNCH                         <V66P11>
               MOVE 'Y'                TO WSAA-REVISIT-CHK-BOX          <V66P11>
           END-IF.                                                      <V66P11>
      *
      * Retrieve the correct edit subroutine for the Subaccount Code
      * being processed.
      *
           PERFORM 2630-READ-T3697.

           IF ITEM-STATUZ              = MRNF
              MOVE F042                TO S2610-SACSCODE-ERR
              GO TO 2620-CURRENCY-AND-RATE
           END-IF.

      * Data will be corrupted if P2610EDCL not on T3697,
      *2067ause of the striclty mandatory switching to P8016            <CS016>
           IF T3697-VALPROG            = SPACES
              MOVE S2610-SACSCODE      TO WSAA-POPUP-SACSCODE
              IF MANDATORY-POPUP
                 MOVE F043             TO S2610-SACSCODE-ERR
              END-IF
              GO TO 2620-CURRENCY-AND-RATE
           END-IF.
      *
           PERFORM 2640-EDIT-ROUTINE.
                                                                        <CS016>
           IF WSAA-SBMACTION          = 'M'                             <CS016>
              GO TO 2618-TRANSACTION-DESC                               <CS016>
           END-IF.                                                      <CS016>
                                                                        <CS016>
      *
      * Pass hidden details and errors detected in the edit routine
      * from linkage fields to screen fields (for Enquiry, only the
      * switching information is relevant).
      *
      ***  IF S2610-INDIC           NOT = 'Y' OR                        <R2-296>
      ***     CSHD-INDIC                = SPACES                        <R2-296>
      ***     MOVE CSHD-INDIC          TO S2610-INDIC                   <R2-296>
      ***  END-IF.                                                      <R2-296>
           MOVE CSHD-INDIC             TO S2610-INDIC.                  <R2-296>
           MOVE CSHD-EXTRA             TO S2610-EXTRA.
           MOVE CSHD-SOMEM             TO S2610-SOMEM.
           MOVE CSHD-AGNTNUM           TO S2610-DISACC.
           MOVE CSHD-SELECT-ERR        TO S2610-SELNUM-ERR.
           MOVE CSHD-BRANCH            TO S2610-BRANCH.                 <V6F114>

           IF SBM-INQUIRY
           OR (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                       <V76F12>
               SBM-MODIFY)                                              <V76F12>
           AND WSAA-SBMACTION          NOT = 'M'                        <CS016>
      ***     OR (SBM-PARTIAL-CANCEL      AND                   <FA2088><R2-296>
      ***        S2610-SELECT-NUMBER    = ZEROS)                <FA2088><R2-296>
              MOVE SPACES              TO S2610-INDIC                   <R2-296>
              GO TO 2628-SELECT
           END-IF.
           IF SBM-PARTIAL-CANCEL          AND                           <FA2088>
              S2610-SELECT-NUMBER       = ZEROS                         <FA2088>
              MOVE SPACES              TO S2610-INDIC                   <FA2088>
              GO                       TO 2628-SELECT                   <FA2088>
           END-IF.                                                      <FA2088>

      *--- During cancellation of receipt that has claims dissection    <FA2303>
      *--- the Edit Routine will always return the latest claim tranno  <FA2303>
      *--- which is not necessarily the tranno corresponding to the     <FA2303>
      *--- dissection being cancelled which causes the message 'Diss    <FA2303>
      *--- amts not = Doc amt' to appear in S8016. Changed to retain    <FA2303>
      *--- the original dissection's tranno for 'CL' dissections.       <FA2303>
      **** MOVE CSHD-TRANNO            TO S2610-TRANNO.                 <FA2303>
           IF NOT (S2610-SACSCODE       = SUBA-CLMCASH-CODE AND         <FA2303>
                   SBM-CANCELLATION)                                    <FA2303>
              MOVE CSHD-TRANNO         TO S2610-TRANNO                  <FA2303>
           END-IF.                                                      <FA2303>
                                                                        <FA2303>
           MOVE CSHD-TRANKEY           TO S2610-TRANKEY.
           MOVE CSHD-CHDRKEY           TO S2610-CHDRKEY.

           MOVE CSHD-TRANAMT-ERR       TO S2610-ORIGAMT-ERR.
           MOVE CSHD-DISSRATE-ERR      TO S2610-SLRATE-ERR.
           MOVE CSHD-TRANDESC-ERR      TO S2610-TRANDESC-ERR.
           MOVE CSHD-ACCNUM-ERR        TO S2610-DISACC-ERR.

           IF S2610-ENTITY-ERR         = SPACES
              MOVE CSHD-TRANKEY-ERR    TO S2610-ENTITY-ERR
           END-IF.
           IF S2610-SACSCODE-ERR       = SPACES
              MOVE CSHD-SACSCODE-ERR   TO S2610-SACSCODE-ERR
           END-IF.
           IF S2610-SACSTYPW-ERR       = SPACES
              MOVE CSHD-SACSTYP-ERR    TO S2610-SACSTYPW-ERR
           END-IF.
           IF S2610-BCURR-ERR          = SPACES
              MOVE CSHD-DISSCURR-ERR    TO S2610-BCURR-ERR
           END-IF.

      * Fund accouting applicable
           IF FDAC-FUNDACCL-YES
              MOVE S2610-SACSCODE     TO WSAA-SACSCODE
              MOVE S2610-SACSTYPW     TO WSAA-SACSTYPW
              MOVE WSAA-ACC-CODE      TO WSAA-ACC-ACCODE
              IF WSAA-VALID-ACCODE    AND
                 WSAA-BANK-FUND    NOT = CSHD-STCDA
              AND CSHD-TRANKEY-ERR     = SPACES                         <FA2479>
      ****       MOVE RPAG            TO S2610-SACSCODE-ERR
      ****                               S2610-SACSTYPW-ERR
                 MOVE RPAG            TO S2610-ENTITY-ERR
              END-IF
           END-IF.
      *
      * Alternatively, default any blank field with existing values
      * passed from the subroutine.
      *
           IF S2610-ORIGAMT            = ZEROES
              MOVE CSHD-ORIGAMT        TO S2610-ORIGAMT
           END-IF.
      *
           IF S2610-BCURR              = SPACES
              MOVE CSHD-ORIGCCY        TO S2610-BCURR
           END-IF.
      *
           IF S2610-ACCTAMT            = ZEROES
              MOVE CSHD-ACCTAMT        TO S2610-ACCTAMT
           END-IF.
      *
           IF S2610-SLRATE             = ZEROES
              MOVE CSHD-DISSRATE       TO S2610-SLRATE
           END-IF.
      *
      *    IF S2610-TRANDESC           = SPACES
      *       MOVE CSHD-TRANDESC       TO S2610-TRANDESC
      *    END-IF.

       2618-TRANSACTION-DESC.
      *
      * Retrieve the Transaction Description (mandatory field).
      *
           IF S2610-SACSCODE-ERR       NOT = SPACES OR
              S2610-SACSTYPW-ERR       NOT = SPACES
              GO TO 2620-CURRENCY-AND-RATE
           END-IF.

           PERFORM 2650-CALL-CKTRDSC.
      *
      *    IF S2610-TRANDESC           = SPACES
      *       MOVE CKDS-TRANDESC       TO S2610-TRANDESC
      *    END-IF.
      *
      *    IF CKDS-STATUZ              = MRNF                           <FA2363>
      *    AND S2610-TRANDESC          = SPACES                         <FA2363>
      *        MOVE H457               TO S2610-TRANDESC-ERR.           <FA2363>
                                                                        <FA2363>
      *    IF CKDS-STATUZ               = O-K                           <FA3131>
      *       MOVE CKDS-TRANDESC       TO S2610-TRANDESC                <FA3131>
      *    END-IF.                                                      <FA3131>
                                                                        <FA3131>
           MOVE  S2610-SACSCODE        TO WSBB-SACSCODE.                <FA3131>
                                                                        <FA3131>
      *    IF CKDS-STATUZ               = O-K                           <FA3131>
      *    AND NOT COMPANY-SUSPENSE                                     <FA3131>
      *       MOVE CKDS-TRANDESC       TO S2610-TRANDESC                <FA3131>
      *    END-IF.                                                      <FA3131>
                                                                        <FA3131>
      *                                                                 <RC002>
      * Read Table 3695 to get the Description of SacsType.             <RC002>
      *                                                                 <RC002>
      *    IF  WSAA-SBMACTION           = 'A'                           <RC002>
           IF  S2610-SACSCODE       NOT = 'SC'                          <RC002>
           AND S2610-SACSTYPW       NOT = ' I' AND 'I '                 <RC002>
              MOVE 'IT'                   TO DESC-DESCPFX               <RC002>
              MOVE WSSP-COMPANY           TO DESC-DESCCOY               <RC002>
              MOVE T3695                  TO DESC-DESCTABL              <RC002>
              MOVE S2610-SACSTYPW         TO DESC-DESCITEM              <RC002>
              MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE              <RC002>
              MOVE READR                  TO DESC-FUNCTION              <RC002>
              MOVE DESCREC                TO DESC-FORMAT                <RC002>
                                                                        <RC002>
              CALL 'DESCIO'            USING DESC-PARAMS                <RC002>
                                                                        <RC002>
              IF DESC-STATUZ           NOT = O-K                        <RC002>
                  MOVE DESC-PARAMS        TO SYSR-PARAMS                <RC002>
                  PERFORM 600-FATAL-ERROR                               <RC002>
              END-IF                                                    <RC002>
                                                                        <RC002>
              MOVE DESC-LONGDESC          TO S2610-TRANDESC             <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
           IF S2610-TRANDESC           = SPACES
              MOVE E186                TO S2610-TRANDESC-ERR
           END-IF.
      *
       2620-CURRENCY-AND-RATE.
      *
      * Validate/default Original Currency and Rate.
      *
           IF S2610-BCURR              = SPACES
              MOVE WSSP-ORIGCCY        TO S2610-BCURR
           END-IF.
      *
           IF S2610-BCURR-ERR          NOT = SPACES
              GO TO 2600-AMOUNTS
           END-IF.
      *
      * Validate that Currency of dissection be Billing currency.
           INITIALIZE                   ITEM-DATA-AREA.
           MOVE SMTP-ITEM            TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY         TO ITEM-ITEMCOY.
           MOVE T3629                TO ITEM-ITEMTABL.
           MOVE S2610-BCURR          TO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.
           IF ITEM-STATUZ            NOT = O-K
              MOVE F955              TO S2610-BCURR-ERR
           ELSE
              MOVE ITEM-GENAREA      TO T3629-T3629-REC
              IF T3629-CRTIND        NOT = 'Y'
                 MOVE F955           TO S2610-BCURR-ERR
              END-IF
           END-IF.
      *
           IF S2610-SLRATE             NOT = ZERO
              GO TO 2624-CHECK-RATE
           ELSE
              IF S2610-BCURR           = WSSP-ORIGCCY
                 MOVE WSSP-SCRATE      TO S2610-SLRATE
                 GO TO 2624-CHECK-RATE
              END-IF
           END-IF.
      *
       2622-GET-RATE.
      *
           INITIALIZE                     CKCU-CHKCURR-REC.
           MOVE 'RATE'                 TO CKCU-FUNCTION.
           MOVE S2610-SLRATE           TO CKCU-RATE.
           MOVE S2610-BCURR            TO CKCU-CURRCODE.
           MOVE WSSP-COMPANY           TO CKCU-COMPANY.
           MOVE WSSP-TRANDATE          TO CKCU-DATE.
           PERFORM 2660-CALL-CHKCURR.

           IF CKCU-STATUZ              NOT = O-K
              MOVE CKCU-STATUZ         TO S2610-SLRATE-ERR
           ELSE
              MOVE CKCU-RATE           TO S2610-SLRATE
           END-IF.

           GO TO 2600-AMOUNTS.
      *
       2624-CHECK-RATE.
      *
           INITIALIZE                     CKCU-CHKCURR-REC.
           MOVE 'RATECHEK'             TO CKCU-FUNCTION.
           MOVE S2610-SLRATE           TO CKCU-RATE.
           MOVE S2610-BCURR            TO CKCU-CURRCODE.
           MOVE WSSP-COMPANY           TO CKCU-COMPANY.
           MOVE WSSP-TRANDATE          TO CKCU-DATE.
           PERFORM 2660-CALL-CHKCURR.

           IF CKCU-STATUZ              NOT = O-K
              MOVE CKCU-STATUZ         TO S2610-SLRATE-ERR
           END-IF.
      *
       2600-AMOUNTS.
      *
      * Computations are irrelevant if the Currency and therefore
      * the Rate are incorrect.
      *
           IF S2610-BCURR-ERR          NOT = SPACES OR
              S2610-SLRATE-ERR         NOT = SPACES
              GO TO 2628-SELECT
           END-IF.
      *                                                                 <V76F06>
      * Perform rounding check for the input amount as well , to ensure <V76F06>
      * it follow the curreny rounding rule that set up by company:     <V76F06>
      *                                                                 <V76F06>
           IF S2610-ORIGAMT        NOT = ZEROES                         <V76F06>
              MOVE S2610-ORIGAMT       TO ZRDP-AMOUNT-IN                <V76F06>
              MOVE S2610-ORIGCURR      TO ZRDP-CURRENCY                 <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              IF S2610-ORIGAMT     NOT = ZRDP-AMOUNT-OUT                <V76F06>
                 MOVE RFIK             TO S2610-ORIGAMT-ERR             <V76F06>
           END-IF.                                                      <V76F06>
      *
      * Amount, Rate, GL Amount: compute the third one with the
      * other two.
      *
           IF S2610-SLRATE             NOT = ZEROES AND
              S2610-ORIGAMT            NOT = ZEROES
              COMPUTE S2610-ACCTAMT ROUNDED =
                       S2610-ORIGAMT * S2610-SLRATE
           END-IF.
                                                                        <V76F06>
           IF S2610-ACCTCCY        NOT = S2610-ORIGCURR                 <V76F06>
              MOVE S2610-ACCTAMT       TO ZRDP-AMOUNT-IN                <V76F06>
              MOVE S2610-ACCTCCY       TO ZRDP-CURRENCY                 <V76F06>
              PERFORM 8000-CALL-ROUNDING                                <V76F06>
              MOVE ZRDP-AMOUNT-OUT     TO S2610-ACCTAMT.                <V76F06>
      *
           IF S2610-ORIGAMT                = ZEROES AND
              S2610-SLRATE             NOT = ZEROES AND
              S2610-ACCTAMT            NOT = ZEROES
              COMPUTE S2610-ORIGAMT ROUNDED =
                       S2610-ACCTAMT / S2610-SLRATE
           END-IF.
      *
           IF S2610-SLRATE                 = ZEROES AND
              S2610-ACCTAMT            NOT = ZEROES AND
              S2610-ORIGAMT            NOT = ZEROES
              COMPUTE S2610-SLRATE ROUNDED =
                       S2610-ACCTAMT  / S2610-ORIGAMT
           END-IF.

      * For partial cancellation , the Original amount
      * and Local Amount can't be less than zeroes.
      *
      *---THIS WILL CAUSE PROBLEM DURING CANCEL OF JOURNAL RECEIPT      <R2-296>
      *      BECAUSE THERE IS NO AMOUNT ENTERED IN THE HEADER SCREEN.   <R2-296>
      ***  IF SBM-PARTIAL-CANCEL  AND                                   <R2-296>
      ***     S2610-SELECT-NUMBER       = 2                             <R2-296>
      ***     IF S2610-ORIGAMT         >= ZEROES                        <R2-296>
      ***        MOVE RF25             TO S2610-ORIGAMT-ERR             <R2-296>
      ***     END-IF                                                    <R2-296>
      ***     IF S2610-ACCTAMT         >= ZEROES                        <R2-296>
      ***        MOVE RF25             TO S2610-ACCTAMT-ERR             <R2-296>
      ***     END-IF                                                    <R2-296>
      ***  END-IF.                                                      <R2-296>
      *
       2628-SELECT.
      * For partial cancellation, user must select at least one subfile
      ***  IF SBM-PARTIAL-CANCEL  AND                                   <R2-296>
      ***     S2610-SELECT-NUMBER        = 2                            <R2-296>
      ***     MOVE 'Y'                  TO WSAA-ONE-SELECTED            <R2-296>
      ***  END-IF.                                                      <R2-296>

      * For Partial Cancellation, the select number must be '2'         <FA3943>
           IF SBM-PARTIAL-CANCEL                                        <FA3943>
              IF S2610-SELECT-NUMBER NOT = ZEROES AND                   <FA3943>
                 S2610-SELECT-NUMBER NOT = 2                            <FA3943>
                 MOVE O001              TO S2610-SELNUM-ERR             <FA3943>
           END-IF.                                                      <FA3943>
      * If there is no PopUp (indicator set in the edit routine)
      * the SELECT field must be blank + go to next paragraph.
           IF CSHD-INDIC                 = SPACES
              IF S2610-SELECT-NUMBER     = 1
                 MOVE O001              TO S2610-SELNUM-ERR
              END-IF
              IF NOT (S2610-SELECT-NUMBER = 2                           <FA5389>
                 AND SBM-PARTIAL-CANCEL)                                <FA5389>
              AND S2610-SELECT-NUMBER NOT = ZEROES                      <FA5389>
                 MOVE O001              TO S2610-SELNUM-ERR             <FA5389>
              END-IF                                                    <FA5389>
              IF SBM-INQUIRY
              OR (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                    <V76F12>
                  SBM-MODIFY)                                           <V76F12>
                 GO TO 2634-UPDATE-SFL-LINE
              ELSE
                 GO TO 2630-GLKEY
              END-IF
           END-IF.

      ***  IF S2610-SELECT-NUMBER        = 1   OR                       <R2-296>
      ***     S2610-INDIC                = '1'                          <R2-296>
      ***     MOVE 'Y'                  TO WSAA-SWITCH-ACTIVE           <R2-296>
      ***  END-IF.                                                      <R2-296>

      * If there is a PopUp, validate the value entered in the SELECT
      * field and check if the user is sanctioned to the option switch
      * authorisation code for the SACSCODE being processed.

           PERFORM 2670-CHCK-SELECTION.
      *
           IF SBM-INQUIRY
           OR (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                       <V76F12>
               SBM-MODIFY)                                              <V76F12>
              GO TO 2634-UPDATE-SFL-LINE
           END-IF.
      *
       2630-GLKEY.
      *
      * Retrieve the General Ledger Key applicable to the dissection.
      *
           IF S2610-ERROR-SUBFILE      NOT = SPACES
              GO TO 2632-MAINTAIN-BALANCE
           END-IF.
      *
           PERFORM 2680-CALL-GLKEY.

           IF GLKY-STATUZ              = 'NFND'
              MOVE E141                TO S2610-SACSCODE-ERR
                                          S2610-SACSTYPW-ERR
           ELSE
           IF GLKY-STATUZ              = 'MRNF'
              MOVE E192                TO S2610-SACSCODE-ERR
                                          S2610-SACSTYPW-ERR
           ELSE
           IF GLKY-STATUZ              = 'NCUR'
              MOVE E193                TO S2610-SACSCODE-ERR
                                          S2610-SACSTYPW-ERR
           ELSE
           IF GLKY-STATUZ              = 'BCTL'
              MOVE E289                TO S2610-SACSCODE-ERR
                                          S2610-SACSTYPW-ERR
           ELSE
           IF GLKY-STATUZ           NOT = O-K
              MOVE GLKY-STATUZ         TO S2610-SACSCODE-ERR
                                          S2610-SACSTYPW-ERR
           END-IF.
      *
      * Update the hidden fields: GL Posting Sign and GL Key
      * if no error is returned.
      *
           IF S2610-SACSCODE-ERR       NOT = SPACES OR
              S2610-SACSTYPW-ERR       NOT = SPACES
              GO TO 2632-MAINTAIN-BALANCE
           END-IF.
      *
           MOVE GLKY-SIGN              TO S2610-GLSIGN.
           MOVE GLKY-CONTOT            TO S2610-CNTTOT.
      *
      **** IF S2610-SACSCODE           = 'GL'                           <V65F04>
           IF S2610-SACSCODE           =  'GL' or 'WH' OR               <V65F04>
                                          'VS' OR 'VP' OR 'VE'          <V65F04>
              MOVE GLKY-GENL-PREFIX    TO WSKY-GENL-GENLPFX
              MOVE GLKY-GENL-COMPANY   TO WSKY-GENL-GENLCOY
              MOVE S2610-GENLCUR       TO WSKY-GENL-GENLCUR
              MOVE S2610-TRANKEY       TO WSAA-TRANKEY
              MOVE WSAA-ENTITY         TO WSKY-GENL-GENLCDE
              MOVE WSAA-GENLKEY        TO S2610-GENLKEY
              GO TO 2632-MAINTAIN-BALANCE
           END-IF.
      *
      * When the Subaccount Type is not GL, call the GL Account
      * Substitution subroutine.
      *
           PERFORM 2690-CALL-GLSUBST.

           MOVE GLKY-GENL-PREFIX       TO WSKY-GENL-GENLPFX.
           MOVE GLKY-GENL-COMPANY      TO WSKY-GENL-GENLCOY.
           MOVE S2610-GENLCUR          TO WSKY-GENL-GENLCUR.
           MOVE GLSB-GLACCT            TO WSKY-GENL-GENLCDE.
           MOVE WSAA-GENLKEY           TO S2610-GENLKEY.
      *
      * Call the edit routine again to load the EXTRA field with all
      * latest dissection values.
      *
           IF SBM-INQUIRY     AND
              MANDATORY-POPUP
              PERFORM 2616-EDIT-ROUTINE
           END-IF.
                                                                        <V76F12>
      *--Receipt modification for non-postdated receipt should behave   <V76F12>
      *--like in inquiry mode.                                          <V76F12>
           IF (WSAA-RBNK-POSTDTEFLG NOT = 'Y' AND                       <V76F12>
               SBM-MODIFY)                                              <V76F12>
           AND MANDATORY-POPUP                                          <V76F12>
              PERFORM 2616-EDIT-ROUTINE                                 <V76F12>
           END-IF.                                                      <V76F12>
      *
       2632-MAINTAIN-BALANCE.
      *
      * Balance should not be maintained when subfile line still in
      * error (correct amounts/rate/glsign required).
      *
           IF S2610-ERROR-SUBFILE      NOT = SPACES
              GO TO 2634-UPDATE-SFL-LINE
           END-IF.
      *
      * Adjust dissection total and hidden field to latest changes in
      * the GL Amount.
      *
           IF SBM-PARTIAL-CANCEL
      *---S2610-CNT CONTAINS THE PREVIOUS SELECTION                     <R2-296>
      *---ADJUSTMENTS ON AMOUNT                                         <R2-296>
              IF S2610-SELECT-NUMBER   NOT = ZEROS AND                  <R2-296>
                 S2610-CNT             NOT = ZEROS                      <R2-296>
                 COMPUTE WSAA-DIFF         = S2610-ACCTAMT - S2610-AMNT <R2-296>
                 MOVE S2610-ACCTAMT       TO S2610-AMNT                 <R2-296>
                 ADD WSAA-DIFF            TO WSAA-DISS-TOTAL            <R2-296>
                ELSE                                                    <R2-296>
      ***     IF S2610-SELECT-NUMBER       = 2                          <R2-296>
      ***        ADD S2610-ACCTAMT        TO WSAA-DISS-TOTAL            <R2-296>
      *---SELECT A PREVIOUSLY NOT SELECTED DISSECTION                   <R2-296>
              IF S2610-SELECT-NUMBER   NOT = ZEROS AND                  <R2-296>
                 S2610-CNT                 = ZEROS                      <R2-296>
                 COMPUTE WSAA-DIFF         = S2610-ACCTAMT - ZEROS      <R2-296>
                 MOVE S2610-ACCTAMT       TO S2610-AMNT                 <R2-296>
                 ADD WSAA-DIFF            TO WSAA-DISS-TOTAL            <R2-296>
                ELSE                                                    <R2-296>
      *---DESELECT A PREVIOUSLY SELECTED DISSECTION                     <R2-296>
              IF S2610-SELECT-NUMBER       = ZEROS AND                  <R2-296>
                 S2610-CNT             NOT = ZEROS                      <R2-296>
                 COMPUTE WSAA-DIFF         = ZEROS - S2610-ACCTAMT      <R2-296>
                 MOVE S2610-ACCTAMT       TO S2610-AMNT                 <R2-296>
                 ADD WSAA-DIFF            TO WSAA-DISS-TOTAL            <R2-296>
              END-IF
           ELSE
              COMPUTE WSAA-DIFF            = S2610-ACCTAMT - S2610-AMNT <R2-296>
              MOVE S2610-ACCTAMT          TO S2610-AMNT                 <R2-296>
      ***     ADD S2610-ACCTAMT           TO WSAA-DISS-TOTAL            <R2-296>
              ADD WSAA-DIFF               TO WSAA-DISS-TOTAL            <R2-296>
           END-IF.
      *                                                                 <R2-296>
      *---FOR PARTIAL CANCELLATION, IT KEEPS TRACK OF NUMBER OF SELECTS <R2-296>
           IF SBM-PARTIAL-CANCEL                                        <R2-296>
              COMPUTE WSAA-DIFF          = S2610-SELECT-NUMBER -        <R2-296>
                                           S2610-CNT                    <R2-296>
              MOVE S2610-SELECT-NUMBER  TO S2610-CNT                    <R2-296>
              ADD WSAA-DIFF             TO WSAA-NUM-SELECTED            <R2-296>
           END-IF.                                                      <R2-296>
      *
      * Now the "confusing stuff": (in fact very simple).
      * The user may have changed the Currency, Amount, or got a new
      * GL Code and therefore GL Sign. Thus the three hidden fields
      * on the subfile, SIGN, ORIGCUR and AMTRI store the last
      * sucessfully edited values. We subtract them from the stored
      * currency values and add the new ones.
      *
           IF S2610-ORIGCUR         NOT = SPACES
               MOVE 'SUBTR'            TO TRNC-FUNCTION
               MOVE S2610-ORIGCUR      TO TRNC-ORIGCURR
               MOVE S2610-AMTRI        TO TRNC-ORIGAMT
               MOVE S2610-SIGN         TO TRNC-GLSIGN

               CALL 'TRNCURBAL' USING TRNC-TRNCURBAL-REC

               IF TRNC-STATUZ       NOT = O-K
                  MOVE 'TRNCURBAL'     TO SYSR-SUBRNAME
                  MOVE TRNC-STATUZ     TO SYSR-STATUZ
                  PERFORM 600-FATAL-ERROR
               END-IF
           END-IF.
      *----Only accumulated the amount for the selected line            <FA3943>
           IF  SBM-PARTIAL-CANCEL                                       <FA3943>
           AND S2610-SELECT-NUMBER      = ZEROS                         <FA3943>
             GO TO 2634-UPDATE-SFL-LINE                                 <FA3943>
           END-IF.                                                      <FA3943>
      *
           MOVE S2610-BCURR            TO S2610-ORIGCUR.
           MOVE S2610-ORIGAMT          TO S2610-AMTRI.
           MOVE S2610-GLSIGN           TO S2610-SIGN.
      *
           MOVE 'ADD  '                TO TRNC-FUNCTION.
           MOVE S2610-ORIGAMT          TO TRNC-ORIGAMT.
           MOVE S2610-BCURR            TO TRNC-ORIGCURR.
           MOVE S2610-GLSIGN           TO TRNC-GLSIGN.

           CALL 'TRNCURBAL'         USING TRNC-TRNCURBAL-REC.

           IF TRNC-STATUZ           NOT = O-K
              MOVE 'TRNCURBAL'         TO SYSR-SUBRNAME
              MOVE TRNC-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
       2634-UPDATE-SFL-LINE.
      *
      * Store last edited values into hidden fields.
      *
           MOVE CSHD-AGNTPFX           TO S2610-ACCPFX.
           MOVE CSHD-AGNTCOY           TO S2610-ACCCOY.
           MOVE CSHD-AGNTNUM           TO S2610-ACCNUM.
                                                                        <V6F114>
           IF   CSHD-BRANCH       NOT =  WSSP-BRANCH  AND               <V6F114>
                CSHD-BRANCH       NOT =  SPACES       AND               <V6F114>
                T3711-SIGN            =  'Y'    AND                     <V6F114>
      ****      NOT SBM-INQUIRY                                 <R2-292><V6F114>
                NOT SBM-INQUIRY                 AND                     <R2-292>
                NOT FIRST-WARNING                                       <R2-292>
                MOVE  'Y'              TO WSAA-INTER-BRANCH-FLAG        <V6F114>
           END-IF.                                                      <V6F114>
                                                                        <PHE010>
           IF  S2610-SACSCODE           = 'LN'                          <PHE010>
               MOVE G317             TO S2610-SACSCODE-ERR              <PHE010>
           END-IF.                                                      <PHE010>
      *
       2670-UPDATE-ERROR-INDICATORS.
      *
           IF S2610-ERROR-SUBFILE      NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
              MOVE 'N'                 TO WSAA-DISPLAY-IMBAL
           ELSE
      * Maintain a count of Dissections entered.
              ADD 1                    TO WSAA-DISS-COUNT
           END-IF.
                                                                        <FA1640>
       2675-UPDATE-SUBFILE.                                             <FA1640>

           MOVE SUPD                   TO SCRN-FUNCTION.
           PERFORM 9000-S2610-IO.
      *
       2680-READ-NEXT-MODIFIED-RECORD.
      *
      ***  MOVE SRDN                   TO SCRN-FUNCTION.                <R2-296>
           MOVE WSAA-SCRN-FUNCTION     TO SCRN-FUNCTION.                <R2-296>
           PERFORM 9000-S2610-IO.
      *
       2690-EXIT.
            EXIT.
      /
       2610-CHECK-T3719-RULES SECTION.
      ********************************
       2610-BEGIN.
           MOVE SPACES                 TO WSAA-RRN (SCRN-SUBFILE-RRN).

           MOVE SPACES                 TO WSKY-ITEM-ITEMITEM.
           STRING WSKY-BATC-BATCTRCDE
                  S2610-SACSCODE
                  S2610-SACSTYPW
           DELIMITED BY SIZE         INTO WSKY-ITEM-ITEMITEM.

           PERFORM 1324-READ-T3719.
           IF ITEM-STATUZ           NOT = O-K
              GO TO 2610-EXIT
           END-IF.

           IF T3719-IND-05          NOT = SPACES AND
              T3719-IND-06          NOT = SPACES AND
              SCRN-SUBFILE-RRN      NOT = 1
              MOVE H166                TO S2610-SACSCODE-ERR
           END-IF.

           IF T3719-IND-07          NOT = SPACES AND
              T3719-IND-08          NOT = SPACES
              MOVE SCRN-SUBFILE-RRN    TO WSAA-RRN (SCRN-SUBFILE-RRN)
           END-IF.

           IF T3719-IND-03          NOT = SPACES AND
              T3719-IND-04          NOT = SPACES
              IF S2610-ENTITY       NOT = SPACES
                 MOVE E436             TO S2610-ENTITY-ERR
              END-IF
           END-IF.
       2610-EXIT.
            EXIT.
      /
       2620-VALIDATE-COMPANY SECTION.
      *******************************
       2620-BEGIN.
      *
      * Check if receipt is possible for the company entered.
      *
           IF WSAA-ENTITY-1            = WSSP-FSUCO OR
                                       = WSSP-COMPANY
              GO TO 2620-EXIT
           END-IF.
      *
           IF WSSP-COMPANY             NOT = WSSP-FSUCO
              MOVE F946                TO S2610-ENTITY-ERR
              GO TO 2620-EXIT
           END-IF.
      *
           IF WSSP-NO-OF-SUB           NOT NUMERIC                      <FA2050>
              MOVE ZEROES              TO WSSP-NO-OF-SUB                <FA2050>
           END-IF.                                                      <FA2050>
      *                                                                 <FA2050>
           PERFORM VARYING WSAA-SUBSCRIPT FROM 1 BY 1
                   UNTIL WSAA-SUBSCRIPT > WSSP-NO-OF-SUB OR
                         WSSP-SUBORCOS (WSAA-SUBSCRIPT) = WSAA-ENTITY-1
                   CONTINUE
           END-PERFORM.

           IF WSAA-SUBSCRIPT            > WSSP-NO-OF-SUB
              MOVE F946                TO S2610-ENTITY-ERR
              GO TO 2620-EXIT
           END-IF.

           MOVE WSSP-SUBORCOS (WSAA-SUBSCRIPT) TO WSAA-TRANCOY.
      *
       2620-EXIT.
            EXIT.
      /
       2630-READ-T3697 SECTION.
      *************************
       2630-BEGIN.
           INITIALIZE                     ITEM-DATA-AREA.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.
           MOVE T3697                  TO ITEM-ITEMTABL.
           MOVE SPACES                 TO ITEM-ITEMITEM.
           STRING WSAA-PROG
                  S2610-SACSCODE
           DELIMITED BY SIZE         INTO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.

           IF ITEM-STATUZ               = O-K
              MOVE ITEM-GENAREA        TO T3697-T3697-REC
           ELSE
              INITIALIZE                  T3697-T3697-REC
           END-IF.
       2630-EXIT.
            EXIT.
      /
       2640-EDIT-ROUTINE SECTION.
      ***************************
       2640-BEGIN.
      *
      * Call the retrieved edit subroutine.
      *
           MOVE SPACES                 TO CSHD-ERRORS.
           MOVE SPACES                 TO CSHD-BRANCH.                  <V6F114>
           MOVE O-K                    TO CSHD-STATUZ.
           MOVE 'CHECK'                TO CSHD-FUNCTION.
           MOVE WSSP-COMPANY           TO CSHD-COMPANY.
           MOVE WSSP-BATCHKEY          TO CSHD-BATCHKEY.
           MOVE WSKY-BATC-BATCTRCDE    TO CSHD-TRANCODE.
      *
           MOVE WSSP-FSUCO             TO CSHD-FSUCO.                   <V72P02>
      *                                                                 <V72P02>
           IF  S2610-SACSCODE          = 'GX'                           <V75G04>
               MOVE WSSP-COMPANY       TO WSAA-TRANCOY                  <V75G04>
           END-IF.                                                      <V75G04>
                                                                        <V75G04>
           MOVE S2610-ENTITY           TO WSAA-ENTITY.
           MOVE WSAA-TRANKEY           TO S2610-TRANKEY.
      *
           IF S2610-BCURR               = SPACES                        <V62P04>
              MOVE WSSP-ORIGCCY        TO S2610-BCURR                   <V62P04>
           END-IF.                                                      <V62P04>

           MOVE WSSP-BANKCODE          TO CSHD-DISSBANK.
           MOVE WSSP-FSUCO             TO CSHD-AGNTCOY.
           MOVE S2610-TRANKEY          TO CSHD-TRANKEY.
           MOVE WSSP-TRANDATE          TO CSHD-TRANDATE.
           MOVE WSAA-TIME              TO CSHD-TRANTIME.
           MOVE VRCM-USER              TO CSHD-USER.
           MOVE S2610-ACCTAMT          TO CSHD-DOCAMT.
           MOVE WSSP-ORIGCCY           TO CSHD-HEDRCURR.
           MOVE S2610-SACSCODE         TO CSHD-SACSCODE.
           MOVE S2610-SACSTYPW         TO CSHD-SACSTYP.
           MOVE S2610-ORIGAMT          TO CSHD-ORIGAMT
                                          CSHD-TRANAMT.
           IF S2610-BCURR              = SPACES                         <FA2148>
              MOVE WSSP-ORIGCCY        TO S2610-BCURR                   <FA2148>
           END-IF.                                                      <FA2148>
           MOVE S2610-BCURR            TO CSHD-ORIGCCY
                                          CSHD-DISSCURR.
           MOVE S2610-SLRATE           TO CSHD-DISSRATE.
           MOVE S2610-ACCTAMT          TO CSHD-ACCTAMT.
           MOVE WSSP-ACCTCCY           TO CSHD-ACCTCCY.
           MOVE SPACES                 TO CSHD-TRANDESC.
           MOVE S2610-EXTRA            TO CSHD-EXTRA.
           MOVE S2610-SOMEM            TO CSHD-SOMEM.
           MOVE S2610-DISACC           TO CSHD-AGNTNUM.
           MOVE S2610-SELECT-NUMBER    TO CSHD-SLT.
           MOVE S2610-INDIC            TO CSHD-INDIC.
           MOVE ZEROES                 TO CSHD-TRANNO.
           MOVE WSSP-DOCTYPE           TO CSHD-PAYTYPE.
           MOVE WSSP-DOCTKEY           TO CSHD-DOCTKEY.
           IF   S2610-TRANSEQ          =  SPACES                        <V71F07>
                MOVE ZEROES            TO CSHD-TRANSEQ                  <V71F07>
           ELSE                                                         <V71F07>
                MOVE S2610-TRANSEQ     TO CSHD-TRANSEQ                  <V71F07>
           END-IF.                                                      <V71F07>
           MOVE PRFX-CASH              TO CSHD-KFK-RDOCPFX.             <V62P04>
           MOVE WSSP-COMPANY           TO CSHD-KFK-RDOCCOY.             <V62P04>
           MOVE S2610-DOCNO            TO CSHD-KFK-RDOCNUM.             <V62P04>
           MOVE S2610-TRANSEQ          TO CSHD-KFK-TRANSEQ.             <V62P04>

           IF S2610-SACSCODE           = 'GR' OR 'GC'
           OR S2610-SACSCODE           = 'CV'                           <V63F29>
           OR S2610-SACSCODE           = 'FL'                           <V66G24>
           OR S2610-SACSCODE           = 'GM'                           <V66G24>
           OR S2610-SACSCODE           = 'GG'                           <V70G03>
           OR S2610-SACSCODE           = 'DN'                           <V71F07>
           OR S2610-SACSCODE           = 'GX'                           <V75G04>
              MOVE WSSP-PTCODE         TO WSAA-PTCODE
              MOVE WSSP-PAYESEL        TO WSAA-PAYESEL
              MOVE WSAA-EXTRA-KEY      TO CSHD-EXTRA
           END-IF.

           MOVE WSSP-LANGUAGE          TO CSHD-LANGUAGE.
           MOVE WSAA-CLNTKEY           to CSHD-CLNTKEY.                 <V76F07>

           CALL T3697-VALPROG       USING CSHD-CASHED-REC.

           IF CSHD-STATUZ           NOT = O-K
              MOVE T3697-VALPROG       TO SYSR-SUBRNAME
              MOVE CSHD-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
       2640-EXIT.
            EXIT.
      /
       2650-CALL-CKTRDSC SECTION.
      ***************************
       2650-BEGIN.
      *
           MOVE 'CHKCS'                TO CKDS-FUNCTION.
           MOVE S2610-SACSTYPW         TO CKDS-TRANTYP .
           MOVE S2610-SACSCODE         TO CKDS-TRANCODE.
           MOVE S2610-TRANDESC         TO CKDS-TRANDESC.
           MOVE WSSP-COMPANY           TO CKDS-COMPANY.
           MOVE WSSP-LANGUAGE          TO CKDS-LANGUAGE.
           MOVE WSAA-CLNTKEY           TO CKDS-CLNTKEY.                 <V76F07>
           MOVE PRFX-CASH              TO CKDS-DOCTYP.                  <V76F07>

           CALL 'CKTRDSC'           USING CKDS-CKTRDSC-REC.

           IF CKDS-STATUZ          NOT = O-K AND MRNF AND NOGO
              MOVE CKDS-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

       2650-EXIT.
            EXIT.
      /
       2660-CALL-CHKCURR SECTION.
      ***************************
       2660-BEGIN.

           CALL 'CHKCURR'           USING CKCU-CHKCURR-REC.

           IF CKCU-STATUZ               = BOMB
              MOVE CKCU-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
       2660-EXIT.
            EXIT.
      /
       2670-CHCK-SELECTION SECTION.
      *****************************
       2670-BEGIN.
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'  AND               <FA4888>
              WSSP-LASTPROG             = 'SR289'                       <FA4888>
              GO TO 2670-EXIT                                           <FA4888>
           END-IF.                                                      <FA4888>
                                                                        <FA4888>
           IF S2610-SELECT-NUMBER       = ZEROES AND
              S2610-INDIC           NOT = '1'
              GO TO 2670-EXIT
           END-IF

           MOVE O-K                    TO OPTS-STATUZ.
           MOVE 'CHCK'                 TO OPTS-FUNCTION.
           MOVE WSAA-PROG              TO OPTS-CALLING-PROG.
           MOVE WSAA-TODAY             TO OPTS-DTEEFF.
           MOVE WSSP-COMPANY           TO OPTS-COMPANY.
           MOVE WSSP-LANGUAGE          TO OPTS-LANGUAGE.
           MOVE WSSP-TRANID            TO VRCM-TRANID.
           MOVE VRCM-USER              TO OPTS-USER.
           IF S2610-SELECT-NUMBER       = ZEROES
              MOVE S2610-INDIC         TO OPTS-SEL-OPTNO
           ELSE
              MOVE S2610-SELECT-NUMBER TO OPTS-SEL-OPTNO
           END-IF.
           MOVE 'L'                    TO OPTS-SEL-TYPE.
           MOVE SPACES                 TO OPTS-SEL-CODE.

           CALL 'OPTSWCH'           USING OPTSWCH-REC
                                          WSSP-SEC-PROGS
                                          WSSP-SEC-ACTNS
                                          WSSP-PROGRAM-PTR
                                          WSSP-FLAG.

           IF OPTS-STATUZ              NOT = O-K
              MOVE OPTS-STATUZ         TO S2610-SELNUM-ERR
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.
       2670-EXIT.
            EXIT.
      /
       2680-CALL-GLKEY SECTION.
      *************************
       2680-BEGIN.
           MOVE 'RETN'                 TO GLKY-FUNCTION.
           MOVE WSSP-COMPANY           TO GLKY-COMPANY.
           MOVE WSKY-BATC-BATCTRCDE    TO GLKY-TRANCODE.
           MOVE S2610-SACSCODE         TO GLKY-SACSCODE.
           MOVE S2610-SACSTYPW         TO GLKY-SACSTYP.
           MOVE SPACES                 TO GLKY-LEDGKEY.

           CALL 'GLKEY'             USING GLKY-GLKEY-REC.

           IF GLKY-STATUZ              = BOMB
              MOVE GLKY-STATUZ         TO SYSR-STATUZ
              MOVE GLKY-GLKEY-REC      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
       2680-EXIT.
            EXIT.
      /
       2690-CALL-GLSUBST SECTION.
      ***************************
       2690-BEGIN.
                                                                        <PHL109>
           MOVE SPACES                 TO CHDRLIF-PARAMS.               <PHL109>
           MOVE WSSP-COMPANY           TO CHDRLIF-CHDRCOY.              <PHL109>
           MOVE S2610-ENTITY           TO CHDRLIF-CHDRNUM.              <PHL109>
           MOVE READR                  TO CHDRLIF-FUNCTION.             <PHL109>
           MOVE CHDRLIFREC             TO CHDRLIF-FORMAT.               <PHL109>
                                                                        <PHL109>
           CALL 'CHDRLIFIO'         USING CHDRLIF-PARAMS.               <PHL109>
           IF  CHDRLIF-STATUZ       NOT = O-K                           <PHL109>
               MOVE SPACES             TO CHDRLIF-CNTTYPE               <PHL109>
           END-IF.                                                      <PHL109>
                                                                        <PHL109>
           MOVE SPACES                 TO GLSB-GLSUBST-REC.
           MOVE 'SBALL'                TO GLSB-FUNCTION.
           MOVE GLKY-COMPANY           TO GLSB-COMPANY.
           MOVE GLKY-GENL-ACCOUNT      TO GLSB-GLACCT.
           MOVE WSSP-BANKCODE          TO GLSB-BANKCODE.
                                                                        <V6F114>
      **** MOVE WSSP-BRANCH            TO GLSB-BRANCH.                  <V6F114>
           IF   CSHD-BRANCH       NOT  =  SPACES                        <V6F114>
                MOVE  CSHD-BRANCH      TO GLSB-BRANCH                   <V6F114>
           ELSE                                                         <V6F114>
                MOVE  WSSP-BRANCH      TO GLSB-BRANCH                   <V6F114>
           END-IF.                                                      <V6F114>

           MOVE CSHD-STCDA             TO GLSB-FUND.
           IF GLSB-FUND                 = SPACE                         <PHL109>
              MOVE CHDRLIF-CNTTYPE     TO GLSB-FUND                     <PHL109>
           END-IF.                                                      <PHL109>
           MOVE ZERO                   TO WSAA-TALLY.                   <PHL109>
           INSPECT GLSB-GLACCT                                          <PHL109>
                   TALLYING WSAA-TALLY FOR ALL '@@@'.                   <PHL109>
           IF WSAA-TALLY                > ZERO                          <PHL109>
              MOVE CHDRLIF-CNTTYPE     TO GLSB-FUND                     <PHL109>
           END-IF.                                                      <PHL109>
                                                                        <PHL109>
           MOVE CSHD-STCDB             TO GLSB-ACCT-CLASS.
           MOVE CSHD-STCDC             TO GLSB-STATCODE-03.
           MOVE CSHD-STCDD             TO GLSB-STATCODE-04.
           MOVE CSHD-STCDE             TO GLSB-STATCODE-05.
           MOVE WSSP-ORIGCCY           TO GLSB-CURRENCY.

           CALL 'GLSUBST'           USING GLSB-GLSUBST-REC.

           IF GLSB-STATUZ           NOT = O-K
              MOVE GLSB-GLSUBST-REC    TO SYSR-PARAMS
              MOVE GLSB-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
       2695-EXIT.
            EXIT.
      /                                                                 <V76F02>
       2700-DOUBLE-CONFIRM SECTION.                                     <V76F02>
      *****************************                                     <V76F02>
       2710-CONFIRM.                                                    <V76F02>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <V76F02>
           OR T3605-CONFIRM         NOT = 'Y'                           <V76F02>
           OR WSSP-EDTERROR         NOT = O-K                           <V76F02>
           OR SCRN-DEVICE-IND           = '*RMT'                        <V76F02>
           OR WSAA-SCRN-STATUZ          = 'SERV'                        <V76F02>
           OR WSAA-SCRN-STATUZ          = 'DELT'                        <V76F02>
           OR WSAA-SCRN-STATUZ          = 'KANJ'                        <V76F02>
           OR NOT (SBM-CREATION                                         <V76F02>
               OR  SBM-CANCELLATION                                     <V76F02>
               OR  SBM-MODIFY                                           <V76F12>
               OR  SBM-PARTIAL-CANCEL)                                  <V76F02>
              GO TO 2790-EXIT.                                          <V76F02>
                                                                        <V76F02>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V76F02>
           PERFORM 9000-S2610-IO.                                       <V76F02>
           PERFORM UNTIL                                                <V76F02>
                   SCRN-STATUZ      NOT = O-K  OR                       <V76F02>
                   S2610-INDIC          = '1'  OR                       <V76F02>
                   S2610-INDIC          = 'A'  OR                       <V76F02>
                   S2610-INDIC          = '2'  OR                       <V76F02>
                   S2610-SELECT-NUMBER  = 1                             <V76F02>
              MOVE SRDN                       TO SCRN-FUNCTION          <V76F02>
              PERFORM 9000-S2610-IO                                     <V76F02>
           END-PERFORM.                                                 <V76F02>
                                                                        <V76F02>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <V76F02>
           IF SCRN-STATUZ           NOT = ENDP                          <V76F02>
              GO 2790-EXIT                                              <V76F02>
           END-IF.                                                      <V76F02>
                                                                        <V76F02>
           IF  S2610-XOPTIND                   = 'X'                    <V76F02>
               GO TO 2790-EXIT                                          <V76F02>
           END-IF.                                                      <V76F02>
                                                                        <V76F02>
           MOVE SPACES                 TO MBOX-CPFMSG                   <V76F02>
                                          MBOX-INSERT                   <V76F02>
                                          MBOX-REPLY                    <V76F02>
                                          MBOX-RESULT.                  <V76F02>
           IF SBM-CREATION                                              <V76F02>
              MOVE TR386-PROGDESC-02   TO MBOX-INSERT                   <V76F02>
              MOVE 'EFA0004'           TO MBOX-CPFMSG                   <V76F02>
           ELSE IF SBM-PARTIAL-CANCEL                                   <V76F02>
              MOVE TR386-PROGDESC-03   TO MBOX-INSERT                   <V76F02>
              MOVE 'EFA0004'           TO MBOX-CPFMSG                   <V76F02>
           ELSE IF SBM-CANCELLATION                                     <V76F02>
              MOVE 'EFA0008'           TO MBOX-CPFMSG                   <V76F02>
           ELSE IF SBM-MODIFY                                           <V76F12>
              MOVE 'EFA0009'           TO MBOX-CPFMSG                   <V76F12>
           END-IF.                                                      <V76F02>
                                                                        <V76F02>
           MOVE T3605-DEFAULT          TO WSAA-MBOX-DFT-VALUE.          <V76F02>
           MOVE WSAA-MBOX-REPLY        TO MBOX-REPLY.                   <V76F02>
                                                                        <V76F02>
           PERFORM 2500-DISPLAY-MSGBOX.                                 <V76F02>
           IF MBOX-REPLY                = 'Y'                           <V76F02>
           AND MBOX-RESULT              = O-K                           <V76F02>
              MOVE O-K                 TO WSSP-EDTERROR                 <V76F02>
           ELSE                                                         <V76F02>
              MOVE 'Y'                 TO WSSP-EDTERROR                 <V76F02>
           END-IF.                                                      <V76F02>
                                                                        <V76F02>
       2790-EXIT.                                                       <V76F02>
           EXIT.                                                        <V76F02>
      /                                                                 <PHLRMS>
       2800-READ-TV041 SECTION.                                         <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       2810-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     RCPYACT-PARAMS  .             <PHLRMS>
           MOVE WSSP-SERVUNIT          TO RCPYACT-RCTYPE  .             <PHLRMS>
           MOVE WSSP-COMMON-AREA(102:8)                                 <PHLRMS>
                                       TO RCPYACT-RCSERNUM.             <PHLRMS>
           MOVE RCPYACTREC             TO RCPYACT-FORMAT  .             <PHLRMS>
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
           IF  RCPYACT-STATUZ          = MRNF                           <PHLRMS>
               GO TO 2890-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           INITIALIZE                     TV041-TV041-REC.              <PHLRMS>
           INITIALIZE                     ITEM-PARAMS.                  <PHLRMS>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <PHLRMS>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <PHLRMS>
           MOVE TV041                  TO ITEM-ITEMTABL.                <PHLRMS>
           MOVE RCPYACT-PURCODE        TO ITEM-ITEMITEM.                <PHLRMS>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <PHLRMS>
           MOVE READR                  TO ITEM-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <PHLRMS>
                                                                        <PHLRMS>
           IF  ITEM-STATUZ             NOT = O-K                        <PHLRMS>
           AND                         NOT = MRNF                       <PHLRMS>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  ITEM-STATUZ             = MRNF                           <PHLRMS>
               GO TO 2890-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE ITEM-GENAREA           TO TV041-TV041-REC.              <PHLRMS>
      *                                                                 <PHLRMS>
       2890-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      *                                                                 <PHLRMS>
       2900-CHECK-SUB-ACC SECTION.                                      <PHLRMS>
      ****************************                                      <PHLRMS>
      *                                                                 <PHLRMS>
       2910-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM 2800-READ-TV041.                                     <PHLRMS>
                                                                        <PHLRMS>
           IF  TV041-TV041-REC     NOT = SPACES                         <PHLRMS>
               IF  S2610-SACSCODE      NOT = SPACES                     <PHLRMS>
               OR  S2610-SACSTYPW      NOT = SPACES                     <PHLRMS>
                   MOVE 'N'            TO WSAA-VALID-SUB-ACC            <PHLRMS>
                   PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 15       <PHLRMS>
                                   OR (TV041-SACSCODE (IDX) = SPACES    <PHLRMS>
                                   AND TV041-SACSTYPE (IDX) = SPACES)   <PHLRMS>
                       IF  S2610-SACSCODE  = TV041-SACSCODE (IDX)       <PHLRMS>
                       AND S2610-SACSTYPW  = TV041-SACSTYPE (IDX)       <PHLRMS>
                           MOVE 'Y'    TO WSAA-VALID-SUB-ACC            <PHLRMS>
                       END-IF                                           <PHLRMS>
                   END-PERFORM                                          <PHLRMS>
      *                                                                 <PHLRMS>
      *--  No parameter, sub-account is valid in default                <PHLRMS>
      *                                                                 <PHLRMS>
                   IF  IDX             = 1                              <PHLRMS>
                       MOVE 'Y'        TO WSAA-VALID-SUB-ACC            <PHLRMS>
                   END-IF                                               <PHLRMS>
                                                                        <PHLRMS>
                   IF  S2610-CONFIRM   NOT = 'Y'                        <PHLRMS>
                   AND WSSP-SERVUNIT   NOT = 'TR'                       <PHLRMS>
                       IF  WSAA-VALID-SUB-ACC  = 'N'                    <PHLRMS>
                           ADD 1       TO WSAA-COUNT-ERR-SUB-ACC        <PHLRMS>
                           MOVE EV48   TO S2610-SACSCODE-ERR            <PHLRMS>
                           MOVE EV48   TO S2610-SACSTYPW-ERR            <PHLRMS>
                       END-IF                                           <PHLRMS>
                       IF  WSAA-POLNO  NOT = SPACES                     <PHLRMS>
                       AND RCPYACT-CHDRNUM NOT = SPACES                 <PHLRMS>
                       AND S2610-ENTITY NOT = RCPYACT-CHDRNUM           <PHLRMS>
                           ADD 1       TO WSAA-COUNT-ERR-POLICY         <PHLRMS>
                           MOVE EV52   TO S2610-ENTITY-ERR              <PHLRMS>
                       END-IF                                           <PHLRMS>
                   END-IF                                               <PHLRMS>
                                                                        <PS008>
                   IF ( WSSP-SBMACTION = 'B'                            <CS009>
                   OR                  = 'A' )                          <CS009>
                   AND WSSP-SERVUNIT   NOT = 'TR'                       <PS008>
                       IF  WSAA-VALID-SUB-ACC  = 'N'                    <PS008>
                           ADD 1       TO WSAA-COUNT-ERR-SUB-ACC        <PS008>
                           MOVE EV48   TO S2610-SACSCODE-ERR            <PS008>
                           MOVE EV48   TO S2610-SACSTYPW-ERR            <PS008>
                       END-IF                                           <PS008>
                       IF  WSAA-POLNO  NOT = SPACES                     <PS008>
                       AND RCPYACT-CHDRNUM NOT = SPACES                 <PS008>
                       AND S2610-ENTITY NOT = RCPYACT-CHDRNUM           <PS008>
                           ADD 1       TO WSAA-COUNT-ERR-POLICY         <PS008>
                           MOVE EV52   TO S2610-ENTITY-ERR              <PS008>
                       END-IF                                           <PS008>
                   END-IF                                               <PS008>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       2990-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      *                                                                 <PHLRMS>
       A2900-GET-POLNO SECTION.                                         <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       A2910-START.                                                     <PHLRMS>
      *                                                                 <PHLRMS>
           IF  S2610-TRANDESC          = SPACES                         <PHLRMS>
               GO TO A2990-EXIT                                         <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           INITIALIZE                     TTRCENQ-PARAMS.               <PHLRMS>
           MOVE S2610-TRANDESC         TO TTRCENQ-TTMPRCNO.             <PHLRMS>
           MOVE TTRCENQREC             TO TTRCENQ-FORMAT.               <PHLRMS>
           MOVE READR                  TO TTRCENQ-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'TTRCENQIO'            USING TTRCENQ-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  TTRCENQ-STATUZ          NOT = O-K                        <PHLRMS>
           AND                         NOT = MRNF                       <PHLRMS>
               MOVE TTRCENQ-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE TTRCENQ-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  TTRCENQ-STATUZ          = MRNF                           <PHLRMS>
               GO TO A2990-EXIT                                         <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE TTRCENQ-CHDRNUM        TO WSAA-POLNO.                   <PHLRMS>
      *                                                                 <PHLRMS>
       A2990-EXIT.                                                      <PHLRMS>
           EXIT.                                                        <PHLRMS>
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
      * No updates if ...
      *  -returning from an optional selection
      *  -(F11 = 'SERV') - inquiry on balance
      *  -(F13 = 'KANJ') - switch to receipt header screen (SR289)
      *  -Switching to PN427 or P8016 is still active (Option 1)
      *
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'    OR
              WSAA-SCRN-STATUZ                 = 'SERV' OR
              WSAA-SCRN-STATUZ                 = 'DELT' OR              <V71F07>
              WSAA-SCRN-STATUZ                 = 'KANJ'                 <R2-296>
      ***     WSAA-SCRN-STATUZ                 = 'KANJ' OR              <R2-296>
      ***     SWITCH-ACTIVE                                             <R2-296>
              IF WSAA-SCRN-STATUZ              = 'KANJ'                 <GP4416>
                 MOVE 'N'                     TO WSAA-SR289-INDIC       <GP4416>
              END-IF                                                    <GP4416>
              GO TO 3090-EXIT
           END-IF.
      *                                                                 <R2-296>
      *---IF SWITCHING REQUIRED, THEN NO UPDATES                        <R2-296>
           MOVE SSTRT                         TO SCRN-FUNCTION.         <R2-296>
           PERFORM 9000-S2610-IO.                                       <R2-296>
           PERFORM UNTIL SCRN-STATUZ       NOT = O-K  OR                <R2-296>
                         S2610-INDIC           = '1'  OR                <V65F04>
                         S2610-INDIC           = 'A'  OR                <FA4032>
                         S2610-INDIC           = '2'  OR                <FA4032>
      ****               S2610-INDIC           = '1'  OR        <V65F04><R2-296>
                         S2610-SELECT-NUMBER   = 1                      <R2-296>
              IF  S2610-SACSCODE               = 'WH'                   <V65F04>
                  MOVE 'Y'                    TO WSAA-PRINT-WH-CERT     <V65F04>
              END-IF                                                    <V65F04>
              MOVE SRDN                       TO SCRN-FUNCTION          <R2-296>
              PERFORM 9000-S2610-IO                                     <R2-296>
           END-PERFORM.                                                 <R2-296>
                                                                        <R2-296>
           IF SCRN-STATUZ                  NOT = ENDP                   <R2-296>
              GO                              TO 3090-EXIT              <R2-296>
           END-IF.                                                      <R2-296>
                                                                        <V66P11>
           IF  S2610-XOPTIND                   = 'X'                    <V66P11>
               GO TO 3090-EXIT                                          <V66P11>
           END-IF.                                                      <V66P11>
      *
      * If Inquiry mode , no updates , but if F7 ('INSR') is pressed
      * then there is updating to RTRN (rewriting the Print Flag)
      *
           IF SBM-INQUIRY                                               <GAPPH1>
      ****    IF WSAA-SCRN-STATUZ      = 'INSR'                         <GAPPH1>
      ****     IF Y-POSTDATED                                           <GAPPH1>
      ****       PERFORM 3700A-PRINT-RECEIPT                            <GAPPH1>
      ****     ELSE                                                     <GAPPH1>
      ****       PERFORM 3700-PRINT-RECEIPT                             <GAPPH1>
      ****     END-IF                                                   <GAPPH1>
      ****    END-IF                                                    <GAPPH1>
              GO TO 3090-EXIT                                           <GAPPH1>
           END-IF.                                                      <GAPPH1>
      *                                                                 <V76F12>
      *    IF SBM-MODIFY                                        <CS016> <V76F12>
           IF WSAA-SBMACTION            = 'K'                           <CS016>
              PERFORM 3200A-UPDATE-RBNKSEQ                              <V76F12>
              IF WSAA-RBNK-POSTDTEFLG   = WSAA-POSTDTEFLG               <V76F12>
                 IF Y-POSTDATED                                         <V76F12>
                    PERFORM 3110A-DELETE-RCPT                           <V76F12>
                    PERFORM 3100A-UPDATE-RCPT                           <V76F12>
      ****          PERFORM 3700A-PRINT-RECEIPT                 <GAPPH1><V76F12>
      ****       ELSE                                           <GAPPH1><V76F12>
      ****          PERFORM 3700-PRINT-RECEIPT                  <GAPPH1><V76F12>
                 END-IF                                                 <V76F12>
              ELSE                                                      <V76F12>
                 IF NOT-POSTDATED                                       <V76F12>
                    PERFORM 3110A-DELETE-RCPT                           <V76F12>
                    PERFORM 3100-UPDATE-BBAL                            <V76F12>
                    PERFORM 3300A-OPEN-BATC-KEY                         <V76F12>
                    PERFORM 3300-CALL-ADDRTRN                           <V76F12>
                    PERFORM 3500-UPDATE-DISSECTION                      <V76F12>
      ****          PERFORM 3700-PRINT-RECEIPT                  <GAPPH1><V76F12>
                    IF SCRN-DEVICE-IND  NOT = '*RMT'                    <V76F12>
                       PERFORM B2000-AT-REQUEST                         <V76F12>
                    END-IF                                              <V76F12>
                 END-IF                                                 <V76F12>
              END-IF                                                    <V76F12>
              PERFORM A700-UPDATE-ZPPR
              PERFORM A600-UNLOCK-DOCUMENT                              <V76F12>
              GO TO 3090-EXIT                                           <V76F12>
           END-IF.                                                      <V76F12>
      *                                                                 <V76F12>
           IF SBM-CREATION                                              <V76F12>
           AND Y-POSTDATED                                              <V76F12>
              PERFORM 3200-CALL-CLTRELN                                 <V76F12>
              PERFORM 3600-UPDATE-RBNK                                  <V76F12>
              PERFORM 3100A-UPDATE-RCPT                                 <V76F12>
      ****    PERFORM 3700A-PRINT-RECEIPT                       <GAPPH1><V76F12>
              GO TO 3090-EXIT                                           <V76F12>
           END-IF.                                                      <V76F12>
      *                                                                 <V76F12>
           IF SBM-CANCELLATION                                          <RC002>
               PERFORM 3800A-PREPRINTED-RECEIPT                         <RC002>
           END-IF.                                                      <RC002>
      *                                                                 <RC002>
           IF (SBM-CANCELLATION OR SBM-PARTIAL-CANCEL)                  <V76F12>
           AND Y-POSTDATED                                              <V76F12>
               PERFORM 3200A-UPDATE-RBNKSEQ                             <V76F12>
               PERFORM 3400A-UPDATE-DISSECTION                          <V76F12>
      ****     PERFORM 3700A-PRINT-RECEIPT                      <GAPPH1><V76F12>
               PERFORM A600-UNLOCK-DOCUMENT                             <V76F12>
              GO TO 3090-EXIT                                           <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <CS016>
           IF WSAA-SBMACTION           = 'M'                            <CS016>
              MOVE SPACES              TO WSAA-UPDATE-FLAG
              PERFORM A800-UPDATE-CLRR-USEED                            <CS016>
              IF CLRRENQ-STATUZ        = O-K                            <CS016>
              AND CLRRENQ-CLNTNUM      NOT = S2610-MLENTITY             <CS016>
                 MOVE 'U'              TO CLRRENQ-USED-TO-BE            <CS016>
                 MOVE WRITD            TO CLRRENQ-FUNCTION              <CS016>
                 MOVE CLRRENQREC       TO CLRRENQ-FORMAT                <CS016>
                 CALL 'CLRRENQIO'      USING CLRRENQ-PARAMS             <CS016>
                 IF CLRRENQ-STATUZ     NOT = O-K                        <CS016>
                    MOVE CLRRENQ-PARAMS  TO SYSR-PARAMS                 <CS016>
                    MOVE CLRRENQ-STATUZ  TO SYSR-STATUZ                 <CS016>
                    PERFORM 600-FATAL-ERROR                             <CS016>
                 END-IF                                                 <CS016>
                 PERFORM 3200-CALL-CLTRELN                              <CS016>
                 MOVE 'Y'              TO WSAA-UPDATE-FLAG
              END-IF                                                    <CS016>
                                                                        <CS016>
              MOVE SPACES              TO WSAA-TRANCODE                 <CS016>
              MOVE S2610-FSACSCODE     TO WSAA-CODE                     <CS016>
              MOVE S2610-FSACSTYP      TO WSAA-TYPE                     <CS016>
              PERFORM A900-READ-RTRN                                    <CS016>
              MOVE WSAA-TRANCODE       TO S2610-TRANSCODE               <CS016>
                                          WSKY-BATC-BATCTRCDE           <CS016>
              PERFORM 1170-LETTER-PROCESSING                            <CS016>
                                                                        <CS016>
              IF RTRNDOC-STATUZ        = O-K                            <CS016>
              AND ( RTRNDOC-RLDGACCT   NOT = S2610-MLENTITY             <CS016>
              OR RTRNDOC-EFFDATE       NOT = WSSP-TRANDATE )            <CS016>
                  MOVE S2610-MLENTITY  TO RTRNDOC-RLDGACCT              <CS016>
                  MOVE S2610-TRANDSC   TO RTRNDOC-TRANDESC              <CS016>
                  IF RTRNDOC-EFFDATE   NOT = WSSP-TRANDATE              <CS016>
                     MOVE WSSP-TRANDATE TO RTRNDOC-EFFDATE              <CS016>
                  END-IF                                                <CS016>
                  PERFORM C900-UPDATE-RTRN                              <CS016>
              END-IF                                                    <CS016>
                                                                        <CS016>
              MOVE SSTRT               TO SCRN-FUNCTION                 <CS016>
              PERFORM 9000-S2610-IO                                     <CS016>
              PERFORM B900-LOOP-SUBFILE-UPDATE UNTIL                    <CS016>
                                               SCRN-STATUZ = ENDP       <CS016>
                                                                        <CS016>
              IF WSAA-UPDATE-FLAG      NOT = 'Y'                        <CS016>
                 GO TO 3090-EXIT                                        <CS016>
              END-IF                                                    <CS016>
              PERFORM 3700-PRINT-RECEIPT                                <CS016>
                                                                        <CS016>
              IF SCRN-DEVICE-IND       NOT = '*RMT'                     <CS016>
                 PERFORM B2000-AT-REQUEST                               <CS016>
              END-IF                                                    <CS016>
                                                                        <CS016>
              GO TO 3090-EXIT                                           <CS016>
           END-IF.                                                      <CS016>
      *
           PERFORM 3100-UPDATE-BBAL.
      *
           PERFORM 3200-CALL-CLTRELN.
      *                                                                 <V76F12>
           PERFORM 3300A-OPEN-BATC-KEY                                  <V76F12>
      *
      * Add a RTRN header-record with TRANSEQ = 0
      *
           PERFORM 3300-CALL-ADDRTRN.
      *
      * Update header TRANDESC of Original Receipt using cancellation
      * receipt details.
      *
      **** IF SBM-CANCELLATION   OR
      ****    SBM-PARTIAL-CANCEL
      ****    PERFORM 3400-REWRT-RDOC
      **** END-IF.
      *
           PERFORM 3500-UPDATE-DISSECTION.
      *
           PERFORM 3600-UPDATE-RBNK.
      *
      * Update ZPPR if Creation or Modify                               <RC002>
      *  If Creation:                                                   <RC002>
      *    Just create new record in ZPPR.                              <RC002>
           IF SBM-CREATION                                              <RC002>
           AND WSSP-SERVUNIT NOT = SPACES                               <RC002>
           AND WSSP-COMMON-AREA(102:8) NOT = SPACES                     <RC002>
               MOVE SPACE              TO ZPPR-PARAMS                   <RC002>
               MOVE PRFX-CASH          TO ZPPR-PREFIX                   <RC002>
               MOVE WSSP-COMPANY       TO ZPPR-COMPANY                  <RC002>
               MOVE WSSP-SERVUNIT      TO ZPPR-RCPREPRTCD               <RC002>
               MOVE WSSP-COMMON-AREA(102:8)                             <RC002>
                                       TO ZPPR-RCPREPRNT                <RC002>
               MOVE S2610-DOCNO        TO ZPPR-RECEIPT                  <RC002>
               MOVE WSSP-BANKCODE      TO ZPPR-BANKCODE                 <RC002>
               MOVE '1'                TO ZPPR-VALIDFLAG                <RC002>
                                                                        <RC002>
               MOVE ZPPRREC            TO ZPPR-FORMAT                   <RC002>
               MOVE WRITR              TO ZPPR-FUNCTION                 <RC002>
                                                                        <RC002>
               CALL 'ZPPRIO'        USING ZPPR-PARAMS                   <RC002>
                                                                        <RC002>
               IF ZPPR-STATUZ       NOT = O-K                           <RC002>
                   MOVE ZPPR-STATUZ         TO SYSR-STATUZ              <RC002>
                   MOVE ZPPR-PARAMS         TO SYSR-PARAMS              <RC002>
                   PERFORM 600-FATAL-ERROR                              <RC002>
               END-IF                                                   <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
           IF  WSAA-SBMACTION          = 'A'                            <PHLRMS>
           OR  WSAA-SBMACTION          = 'I'                            <PHLRMS>
           OR  WSAA-SBMACTION          = 'B'                            <PS008>
               IF  WSSP-SERVUNIT        NOT = SPACES                    <PHLRMS>
               AND WSSP-COMMON-AREA(102:8) NOT = SPACES                 <PHLRMS>
                   PERFORM 4500-UPDATE-TRRN                             <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-SBMACTION          = 'A' OR 'I'                     <PHE003>
           OR WSAA-SBMACTION           = 'B'                            <PS008>
               IF  WSSP-SERVUNIT        NOT = SPACES                    <PHE003>
               AND WSSP-COMMON-AREA(102:8) NOT = SPACES                 <PHE003>
                   PERFORM 4700-UPDATE-RCOL                             <PHE003>
               END-IF                                                   <PHE003>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE SPACES                 TO WSSP-SERVUNIT                 <RC002>
                                          WSSP-COMMON-AREA(102:8) .     <RC002>
      *
           PERFORM 3700-PRINT-RECEIPT.
      *
           IF SCRN-DEVICE-IND       NOT = '*RMT'                        <V62P15>
              PERFORM B2000-AT-REQUEST                                  <V62P15>
           END-IF.                                                      <V62P15>
      *                                                                 <V76F12>
           PERFORM A600-UNLOCK-DOCUMENT.                                <V76F12>
      *                                                                 <V62P15>
       3090-EXIT.
            EXIT.
      /
       3100-UPDATE-BBAL SECTION.
      **************************
       3100-BEGIN.
      *
      * Add the total of money going into the Bank Code to the total
      * stored on the corresponding record of the Bank Balance file
      * (if not found, add a new record).
      *
           INITIALIZE                     BBAL-DATA-AREA.
           MOVE SMTP-BBAL              TO BBAL-BBALPFX.
           MOVE WSSP-COMPANY           TO BBAL-BBALCOY.
           MOVE WSSP-BANKCODE          TO BBAL-BBALCODE.
           PERFORM B1030-READH-BBAL.

           IF BBAL-STATUZ               = MRNF
              MOVE ZEROES              TO BBAL-BBALBAL
              MOVE WRITR               TO BBAL-FUNCTION
           ELSE
              MOVE REWRT               TO BBAL-FUNCTION
           END-IF.

           MOVE WSAA-TODAY             TO BBAL-BBALTDTE.
           MOVE WSAA-TIME              TO BBAL-BBALTIME.

           IF WSSP-SIGN                 = '+' OR SPACES
              ADD WSSP-DOCORIGAMT      TO BBAL-BBALBAL
           ELSE
              SUBTRACT WSSP-DOCORIGAMT
                                     FROM BBAL-BBALBAL
           END-IF.

           CALL 'BBALIO'            USING BBAL-PARAMS.
           IF BBAL-STATUZ           NOT = O-K
              MOVE BBAL-STATUZ         TO SYSR-STATUZ
              MOVE BBAL-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
       3100-EXIT.
            EXIT.
      /
       3200-CALL-CLTRELN SECTION.
      **************************
       3200-BEGIN.
      *
      * Add - or confirm - client relationship.
      *
           MOVE SPACES                 TO CLRN-CLTRELN-REC.
           MOVE 'ADD  '                TO CLRN-FUNCTION.
           MOVE WSAA-CLNTKEY           TO CLRN-CLNTKEY.
           MOVE CLRF-CASF              TO CLRN-CLRRROLE.
      *
      * Here we have to move some values to FOREPFX, FORECOY and
      * FORENUM otherwise CLRRPF won't be updated properly.
      *
           IF CLRN-CLRRROLE            = 'CF'
              MOVE WSAA-RECEIPT-PFX    TO CLRN-FOREPFX
              MOVE WSAA-RECEIPT-COY    TO CLRN-FORECOY
              MOVE WSAA-NEW-RECEIPT    TO CLRN-FORENUM
           ELSE
              MOVE SPACES              TO CLRN-FOREKEY
           END-IF.
      *
           CALL 'CLTRELN'           USING CLRN-CLTRELN-REC.

           IF  CLRN-STATUZ             NOT = O-K AND F962
               MOVE CLRN-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.
       3200-EXIT.
            EXIT.
      /
       3300-CALL-ADDRTRN SECTION.
      ***************************
       3300-BEGIN.

           INITIALIZE                     ADDR-ADDRTRN-REC.
           MOVE '0000'                 TO ADDR-TRANSEQ.
           MOVE 'NPSTW'                TO ADDR-FUNCTION.

           MOVE WSSP-BATCHKEY          TO ADDR-BATCKEY.
           MOVE WSKY-BATC-BATCACTMN    TO ADDR-POSTMONTH.
           MOVE WSKY-BATC-BATCACTYR    TO ADDR-POSTYEAR.
           MOVE WSAA-NEW-RECEIPT       TO WSAA-RECEIPT.
           MOVE WSAA-DOCTKEY           TO ADDR-RDOCKEY.
           MOVE WSSP-GENLKEY           TO ADDR-GENLKEY.
           MOVE WSSP-ACCTCCY           TO ADDR-GENLCUR.
           MOVE WSSP-CONTOT            TO ADDR-CONTOT.
           MOVE WSSP-SIGN              TO ADDR-GLSIGN.
           IF WSSP-PTCODE              = CSRF-AG
              MOVE WSSP-PTCODE         TO WSKY-AGNT-AGNTPFX
              MOVE WSSP-COMPANY        TO WSKY-AGNT-AGNTCOY
              MOVE WSSP-PAYESEL        TO WSKY-AGNT-AGNTNUM
              MOVE WSAA-AGNTKEY        TO ADDR-RLDGKEY
           ELSE
              MOVE WSAA-CLNTKEY        TO ADDR-RLDGKEY
           END-IF.
           MOVE WSSP-PTCODE            TO ADDR-SACSCODE.
           MOVE WSSP-DOCTYPE           TO ADDR-SACSTYP .
           MOVE WSSP-BANKCODE          TO ADDR-BANKCODE.
           MOVE WSSP-TRANDATE          TO ADDR-EFFDATE.
           MOVE SPACES                 TO ADDR-BANKKEY.
           MOVE WSSP-DOCORIGAMT        TO ADDR-ORIGAMT.
           MOVE WSSP-DOCACCTAMT        TO ADDR-ACCTAMT.
           MOVE WSSP-ORIGCCY           TO ADDR-ORIGCCY.
           MOVE WSSP-SCRATE            TO ADDR-CURRRATE.
      *
           MOVE WSSP-PAYENME           TO DBCS-INPUT-STRING.
           MOVE WSAA-TRANDESC-LEN      TO DBCS-OUTPUT-LENGTH.

           MOVE SPACES                 TO DBCS-STATUZ.
           CALL 'DBCSTRNC'          USING DBCSTRNC-REC.

           IF DBCS-STATUZ           NOT = O-K
              MOVE DBCS-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE DBCS-OUTPUT-STRING     TO ADDR-TRANDESC.

           CALL 'ADDRTRN'           USING ADDR-ADDRTRN-REC.

           IF ADDR-STATUZ           NOT = O-K
              MOVE ADDR-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

       3300-EXIT.
            EXIT.
      /
       3400-REWRT-RDOC SECTION.
      *************************
       3400-BEGIN.

           MOVE SPACES                 TO RDOC-DATA-KEY.
           MOVE PRFX-CASH              TO RDOC-RDOCPFX.
           MOVE WSSP-COMPANY           TO RDOC-RDOCCOY.
           MOVE WSAA-RECEIPT           TO RDOC-RDOCNUM.
           MOVE '0000'                 TO RDOC-TRANSEQ.
           MOVE READH                  TO RDOC-FUNCTION.
           MOVE RDOCREC                TO RDOC-FORMAT.
           CALL 'RDOCIO'            USING RDOC-PARAMS.
           IF RDOC-STATUZ           NOT = O-K
              MOVE RDOC-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE WSSP-BATCHKEY          TO WSAA-CANL-BATCKEY.
           MOVE SPACES                 TO RDOC-TRANDESC.
           STRING WSAA-CANL-BATCTRCDE
                  PRFX-CASH
                  WSSP-COMPANY
                  WSAA-NEW-RECEIPT  DELIMITED BY SIZE
                                     INTO RDOC-TRANDESC.

           MOVE REWRT                  TO RDOC-FUNCTION.
           CALL 'RDOCIO'            USING RDOC-PARAMS.
           IF RDOC-STATUZ           NOT = O-K
              MOVE RDOC-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
       3400-EXIT.
            EXIT.
      /
       3500-UPDATE-DISSECTION SECTION.
      *******************************
       3500-BEGIN.
      *
           MOVE ZEROES                 TO WSAA-TRANSEQ.

           MOVE SSTRT                  TO SCRN-FUNCTION.
           PERFORM 9000-S2610-IO.

           PERFORM 3510-LOOP-SUBFILE
                                       UNTIL SCRN-STATUZ = ENDP.
      *
       3500-EXIT.
            EXIT.
      /
       3510-LOOP-SUBFILE SECTION.
      ***************************
       3510-BEGIN.
      *
      * For partial cancellation , only create RBNK record
      * for those selected subfile only
           IF SBM-PARTIAL-CANCEL          AND
              S2610-SELECT-NUMBER   NOT = 2
              GO TO 3510-READ-NEXT-SUBFILE
           END-IF.

      *                                                                 <V62P04>
      *---KFK IS APPLICABLE ONLY FOR CLAIMS                             <V62P04>
           IF S2610-SACSCODE       NOT = 'CL'                           <V62P04>
           AND WSAA-P2200UPKK-CALL     = 'Y'                            <V63P28>
              PERFORM 3800-DELETE-PKFK                                  <V62P04>
           END-IF.                                                      <V62P04>
      *                                                                 <V62P04>
           IF S2610-SACSCODE           = SPACES
              GO TO 3510-READ-NEXT-SUBFILE
           END-IF.
      *
      * Update database with SFL data (TRANSEQ incremented) via
      * update subroutine.
      *
           PERFORM 3511-READ-T3697.

           PERFORM 3512-CALL-UPDATE-ROUTINE.
                                                                        <V62P04>
      *---BECAUSE RTRN IS RESEQUENCED DURING THE UPDATE, PKFK           <V62P04>
      *     ALSO NEEDS TO BE RESEQUENCED                                <V62P04>
           IF S2610-SACSCODE              = 'CL'                        <V62P04>
           AND WSAA-P2200UPKK-CALL     = 'Y'                            <V63P28>
              PERFORM 3900-UPDATE-KFK                                   <V62P04>
           END-IF.                                                      <V62P04>

      * Receipt cancellation & Partial cancellation                     <FA3384>
      * get back the original receipt document to update the            <FA3384>
      * sub ledger account.                                             <FA3384>
                                                                        <FA3384>
           IF S2610-SACSCODE           = 'SC'                           <FA3384>
              IF SBM-PARTIAL-CANCEL    OR                               <FA3384>
                 SBM-CANCELLATION                                       <FA3384>
                 PERFORM B3000-UP-ADOC.                                 <FA3384>
      *                                                                 <FA3384>
       3510-READ-NEXT-SUBFILE.
      *
           MOVE SRDN                   TO SCRN-FUNCTION.
           PERFORM 9000-S2610-IO.
       3510-EXIT.
            EXIT.
      /
       3511-READ-T3697 SECTION.
      *************************
       3511-BEGIN.
      *
      * Retrieve the correct update subroutine for the
      * Subaccount Code being processed.
      *
           IF S2610-SACSCODE            = SPACES                        <V76F07>
              GO TO 3511-EXIT                                           <V76F07>
           END-IF                                                       <V76F07>
           INITIALIZE                     ITEM-DATA-AREA.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE WSSP-FSUCO             TO ITEM-ITEMCOY.
           MOVE T3697                  TO ITEM-ITEMTABL.
           MOVE SPACES                 TO ITEM-ITEMITEM.
           STRING WSAA-PROG
                  S2610-SACSCODE
           DELIMITED BY SIZE         INTO ITEM-ITEMITEM.
           PERFORM B1000-READR-ITEM.

           IF ITEM-STATUZ           NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE ITEM-GENAREA           TO T3697-T3697-REC.

           IF T3697-UPDPROG            = SPACES
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              MOVE F044                TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
       3511-EXIT.
            EXIT.
      /
       3512-CALL-UPDATE-ROUTINE SECTION.
      **********************************
       3512-BEGIN.
      *
           ADD  1                      TO WSAA-TRANSEQ.
      *
      * Pass all details to subroutine linkage copybook
      *
           MOVE O-K                    TO CSHD-STATUZ.
           MOVE 'UPDAT'                TO CSHD-FUNCTION.
           MOVE WSAA-TRANSEQ           TO CSHD-TRANSEQ.
           MOVE WSSP-COMPANY           TO CSHD-COMPANY.
           MOVE WSSP-TRANID            TO CSHD-TRANID.
           MOVE WSSP-BATCHKEY          TO CSHD-BATCHKEY.
           MOVE WSAA-NEW-RECEIPT       TO WSAA-RECEIPT.
           MOVE WSAA-DOCTKEY           TO CSHD-DOCTKEY.
           MOVE WSAA-CLNTKEY           TO CSHD-CLNTKEY.
           MOVE GTCL-ACCTKEY           TO CSHD-AGNTKEY.
           MOVE WSSP-TRANDATE          TO CSHD-TRANDATE.
           MOVE WSAA-TIME              TO CSHD-TRANTIME.
           MOVE VRCM-USER              TO CSHD-USER.
           MOVE WSSP-DOCTYPE           TO CSHD-PAYTYPE.
           MOVE WSSP-ORIGCCY           TO CSHD-HEDRCURR.
           MOVE WSSP-BANKCODE          TO CSHD-DISSBANK.
           MOVE S2610-SACSCODE         TO CSHD-SACSCODE.
           MOVE S2610-SACSTYPW         TO CSHD-SACSTYP.
           IF SBM-CANCELLATION         AND                              <FA3384>
              COMPANY-SUSPENSE                                          <FA3384>
              MOVE WSAA-OLD-RECEIPT    TO WSAA-WENTITY                  <FA3384>
              MOVE WSAA-WTRANKEY       TO CSHD-TRANKEY                  <FA3384>
           ELSE                                                         <FA3384>
           MOVE S2610-TRANKEY          TO CSHD-TRANKEY.
           MOVE S2610-ORIGAMT          TO CSHD-ORIGAMT.
           MOVE S2610-ACCTAMT          TO CSHD-ACCTAMT.
           MOVE S2610-BCURR            TO CSHD-ORIGCCY.
           MOVE S2610-GENLCUR          TO CSHD-ACCTCCY.
           MOVE S2610-SLRATE           TO CSHD-DISSRATE.
           MOVE S2610-TRANDESC         TO CSHD-TRANDESC.
           MOVE S2610-GENLKEY          TO CSHD-GENLKEY.
           MOVE S2610-GLSIGN           TO CSHD-SIGN.
           MOVE S2610-CNTTOT           TO CSHD-CONTOT.
           MOVE S2610-CHDRKEY          TO CSHD-CHDRKEY.
                                                                        <V74G02>
      * Pass old receipt number for receipt cancellation.               <V74G02>
      ***  IF S2610-SACSCODE           = 'FL'                           <V74G02>
      ***  IF (S2610-SACSCODE          = 'FL' OR 'GM')          <V75G04><V74G02>
           IF  S2610-SACSCODE          = 'FL'                           <V75G04>
           OR  S2610-SACSCODE          = 'GM'                           <V75G04>
           OR  S2610-SACSCODE          = 'GX'                           <V75G04>
               IF SBM-PARTIAL-CANCEL    OR                              <V74G02>
                  SBM-CANCELLATION                                      <V74G02>
                  MOVE WSAA-OLD-RECEIPT TO CSHD-CHDRNUM                 <V74G02>
               ELSE                                                     <GP5380>
                  MOVE SPACES           TO CSHD-CHDRNUM                 <GP5380>
               END-IF                                                   <V74G02>
           END-IF.                                                      <V74G02>
                                                                        <V74G02>
           MOVE S2610-TRANNO           TO CSHD-TRANNO.
           MOVE S2610-EXTRA            TO CSHD-EXTRA.
           MOVE S2610-SOMEM            TO CSHD-SOMEM.
           MOVE S2610-SELECT-NUMBER    TO CSHD-SLT.
           MOVE S2610-INDIC            TO CSHD-INDIC.
           MOVE S2610-ACCPFX           TO CSHD-AGNTPFX.
           MOVE S2610-ACCCOY           TO CSHD-AGNTCOY.
           MOVE S2610-ACCNUM           TO CSHD-AGNTNUM.
           IF S2610-SACSCODE           = 'TP'
              MOVE S2610-DISACC        TO CSHD-AGNTNUM
           END-IF.

           MOVE WSSP-LANGUAGE          TO CSHD-LANGUAGE.
           MOVE S2610-BRANCH           TO CSHD-BRANCH.                  <V6F114>
           MOVE PRFX-CASH              TO CSHD-KFK-RDOCPFX.             <V62P04>
           MOVE WSSP-COMPANY           TO CSHD-KFK-RDOCCOY.             <V62P04>
           MOVE S2610-DOCNO            TO CSHD-KFK-RDOCNUM.             <V62P04>
           MOVE S2610-TRANSEQ          TO CSHD-KFK-TRANSEQ.             <V62P04>

           IF S2610-SACSCODE           = 'GC'
           OR S2610-SACSCODE           = 'CV'                           <V63F29>
              MOVE WSSP-PTCODE         TO WSAA-PTCODE
              MOVE WSSP-PAYESEL        TO WSAA-PAYESEL
              MOVE WSAA-EXTRA-KEY      TO CSHD-EXTRA
           END-IF.

           CALL T3697-UPDPROG       USING CSHD-CASHED-REC.

           IF CSHD-STATUZ             NOT = O-K
              MOVE T3697-UPDPROG       TO SYSR-SUBRNAME
              MOVE CSHD-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

       3512-EXIT.
            EXIT.
      /
       3600-UPDATE-RBNK SECTION.
      **************************
       3600-BEGIN.
      *
           MOVE RBNKREC                TO RBNK-FORMAT.
           MOVE RLSE                   TO RBNK-FUNCTION.
           CALL 'RBNKIO'            USING RBNK-PARAMS.

           IF  RBNK-STATUZ          NOT = O-K
               MOVE RBNK-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
           MOVE SCRN-SUBFILE-RRN       TO WSAA-SFL-RRN.
           MOVE SCRN-SUBFILE-END       TO WSAA-SFL-END.
      *
           MOVE WSAA-SR289-SFL-END     TO SCRN-SUBFILE-END.
           MOVE SCRN-SCRNAME           TO WSAA-SCRN-SCRNAME.

           MOVE SSTRT                  TO SCRN-FUNCTION.
           PERFORM 9100-SR289-IO.

           MOVE ZEROES                 TO WSAA-SEQNBR.                  <V76F12>
           PERFORM 3610-LOOP-SR289-SUBFILE
                                    UNTIL SCRN-STATUZ = ENDP

           MOVE WSAA-SCRN-SCRNAME      TO SCRN-SCRNAME.
      *
           MOVE WSAA-SFL-RRN           TO SCRN-SUBFILE-RRN.
           MOVE WSAA-SFL-END           TO SCRN-SUBFILE-END.

       3600-EXIT.
            EXIT.
      /
       3610-LOOP-SR289-SUBFILE SECTION.
      *********************************
       3610-BEGIN.
      * For partial cancellation , only create RBNK record
      * for those selected subfile
           IF SBM-PARTIAL-CANCEL          AND
              SR289-SELECT              = SPACES
              GO TO 3610-READ-NEXT-SUBFILE
           END-IF.

           IF SR289-PAYTYPE             = SPACES
              GO TO 3610-READ-NEXT-SUBFILE
           END-IF.

           ADD 1                       TO WSAA-SEQNBR.                  <V76F12>
           INITIALIZE                     RBNK-DATA-AREA.
           MOVE PRFX-CASH              TO RBNK-RDOCPFX.
           MOVE WSSP-COMPANY           TO RBNK-RDOCCOY.
           MOVE WSAA-NEW-RECEIPT       TO RBNK-RDOCNUM.
           MOVE SR289-CHEQNO           TO RBNK-CHQNUM.
           MOVE SR289-TCHQDATE         TO RBNK-TCHQDATE.
           MOVE SR289-BANKDESC-01      TO RBNK-BANKDESC01.
           MOVE SR289-BANKDESC-02      TO RBNK-BANKDESC02.
           MOVE SR289-BANKDESC-03      TO RBNK-BANKDESC03.
           MOVE SPACES                 TO RBNK-RCPTREV.
           MOVE SR289-ZCHQTYP          TO RBNK-ZCHQTYP.
           MOVE SR289-PAYTYPE          TO RBNK-PAYTYPE.
           MOVE SR289-BANKKEY          TO RBNK-BANKKEY.
      **** MOVE SR289-DOCACCTAMT       TO RBNK-DOCORIGAMT.              <FA2442>
           MOVE SR289-DOCORIGAMT       TO RBNK-DOCORIGAMT.              <FA2442>
           MOVE SR289-ORIGCURR         TO RBNK-ORIGCCY.
           MOVE SR289-SCRATE           TO RBNK-SCRATE.
           MOVE SR289-DOCACCTAMT       TO RBNK-DOCACCTAMT.
           MOVE SR289-ACCTCCY          TO RBNK-ACCTCCY.
           MOVE WSAA-MARRYFLAG         TO RBNK-MARRYFLAG.               <V64F13>
                                                                        <V76F13>
      *-- Credit Card Details                                           <V76F13>
           MOVE SR289-CRCARDMOD        TO RBNK-CRCARDMOD.               <V76F13>
           MOVE SR289-CCMID            TO RBNK-CCMID.                   <V76F13>
           MOVE SR289-CCTID            TO RBNK-CCTID.                   <V76F13>
           MOVE SR289-MCHNTID          TO RBNK-MCHNTID.                 <V76F13>
           MOVE SR289-TRMNLID          TO RBNK-TRMNLID.                 <V76F13>
           MOVE SR289-CRCARDTYPE       TO RBNK-CRCARDTYPE.              <V76F13>
           MOVE SR289-CRDTCARD         TO RBNK-CRDTCARD.                <V76F13>
           MOVE SR289-AUTHID           TO RBNK-AUTHID.                  <V76F13>
           MOVE SR289-CRCNAME          TO RBNK-CRCNAME.                 <V76F13>
           MOVE SR289-CRCARDEXPY       TO RBNK-CRCARDEXPY.              <V76F13>
           MOVE SR289-CRCARDEXPM       TO RBNK-CRCARDEXPM.              <V76F13>
                                                                        <V76F13>
           MOVE SPACES                 TO RBNK-HLRSNCD                  <V76F12>
                                          RBNK-CNRSNCD                  <V76F12>
                                          RBNK-INSREFNO.                <V76F12>
           MOVE ZEROES                 TO RBNK-CRCARDCHG                <V76F13>
                                          RBNK-DISHONNO.                <V76F13>
           MOVE VRCM-MAX-DATE          TO RBNK-AUTHDAT                  <V76F12>
                                          RBNK-REALDAT.                 <V76F12>
           MOVE SR289-RCPTSTAT         TO RBNK-RCPTSTAT.                <V76F12>
           MOVE SR289-DEPDATE          TO RBNK-DEPDATE.                 <V76F12>
           MOVE WSSP-CHEQ-PRESFLAG     TO RBNK-POSTDTEFLG.              <V76F12>
           MOVE WSAA-TODAY             TO RBNK-TRANSACTION-DATE.        <V76F12>
           MOVE WSAA-TIME              TO RBNK-TRANSACTION-TIME.        <V76F12>
           MOVE VRCM-USER              TO RBNK-USER.                    <V76F12>
           MOVE WSAA-TODAY             TO RBNK-CRTDATE.                 <V76F12>
           MOVE WSAA-SEQNBR            TO RBNK-SEQNBR.                  <V76F12>

           IF (SBM-CANCELLATION OR SBM-PARTIAL-CANCEL)                  <V76F12>
           AND RCTSTATFLG                                               <V76F12>
              MOVE RCTST-RR            TO RBNK-RCPTSTAT                 <V76F12>
           END-IF.                                                      <V76F12>
      * Update INSREFNO                                                 <RC002>
           MOVE SR289-INSREFNO         TO RBNK-INSREFNO.                <RC002>
      *    MOVE SR289-BANKREFNO        TO RBNK-INSREFNO.                <RC002>
                                                                        <V76F12>
           MOVE WRITR                  TO RBNK-FUNCTION.
           MOVE RBNKREC                TO RBNK-FORMAT.

           CALL 'RBNKIO'            USING RBNK-PARAMS.

           IF RBNK-STATUZ           NOT = O-K
              MOVE RBNK-PARAMS         TO SYSR-PARAMS
              MOVE RBNK-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
                                                                        <V73G32>
      * Write CHQR when cheque payment.                                 <V73G32>
           IF SR289-PAYTYPE             = '2'                           <V73G32>
              AND TR9QK-VALIDCHQ        = 'Y'                           <V73G34>
              INITIALIZE                  CHQR-DATA-AREA                <V73G32>
              MOVE WSSP-COMPANY        TO CHQR-RLDGCOY                  <V73G32>
              MOVE WSAA-NEW-RECEIPT    TO CHQR-RDOCNUM                  <V73G32>
              MOVE SR289-CHEQNO        TO CHQR-CHQNUM                   <V73G32>
              MOVE SR289-TCHQDATE      TO CHQR-TCHQDATE                 <V73G32>
              MOVE VRCM-MAX-DATE       TO CHQR-RELDTE                   <V73G32>
              MOVE 'N'                 TO CHQR-REAFLG                   <V73G32>
                                                                        <V73G32>
              MOVE CHQRREC             TO CHQR-FORMAT                   <V73G32>
              MOVE WRITR               TO CHQR-FUNCTION                 <V73G32>
              CALL 'CHQRIO'         USING CHQR-PARAMS                   <V73G32>
                                                                        <V73G32>
              IF CHQR-STATUZ        NOT = O-K                           <V73G32>
                 MOVE CHQR-PARAMS      TO SYSR-PARAMS                   <V73G32>
                 MOVE CHQR-STATUZ      TO SYSR-STATUZ                   <V73G32>
                 PERFORM 600-FATAL-ERROR                                <V73G32>
              END-IF                                                    <V73G32>
           END-IF.                                                      <V73G32>

      * Update Reverse Document No.
           IF SBM-CANCELLATION   OR
              SBM-PARTIAL-CANCEL

              PERFORM 3620-REWRT-RBNK

           END-IF.

       3610-READ-NEXT-SUBFILE.
           MOVE SRDN                   TO SCRN-FUNCTION.
           PERFORM 9100-SR289-IO.
       3610-EXIT.
            EXIT.
      /
       3620-REWRT-RBNK SECTION.
      *************************
       3620-BEGIN.
           INITIALIZE                     RBNK-DATA-AREA.
           MOVE O-K                    TO RBNK-STATUZ.
           MOVE PRFX-CASH              TO RBNK-RDOCPFX.
           MOVE WSSP-COMPANY           TO RBNK-RDOCCOY.
           MOVE WSAA-OLD-RECEIPT       TO RBNK-RDOCNUM.
           MOVE BEGN                   TO RBNK-FUNCTION.
           MOVE RBNKREC                TO RBNK-FORMAT.

           PERFORM 3621-LOOP-RBNK   UNTIL RBNK-STATUZ = ENDP.
       3620-EXIT.
            EXIT.
      /
       3621-LOOP-RBNK SECTION.
      ************************
       3621-BEGIN.

           CALL 'RBNKIO'            USING RBNK-PARAMS.
           IF RBNK-STATUZ           NOT = O-K AND ENDP
              MOVE RBNK-STATUZ         TO SYSR-STATUZ
              MOVE RBNK-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF RBNK-STATUZ               = ENDP
           OR RBNK-RDOCPFX          NOT = PRFX-CASH
           OR RBNK-RDOCCOY          NOT = WSSP-COMPANY
           OR RBNK-RDOCNUM          NOT = WSAA-OLD-RECEIPT
              MOVE ENDP                TO RBNK-STATUZ
              GO TO 3621-EXIT
           END-IF.
      *
           IF SR289-CHEQNO              = SPACES
              IF  FIRST-READ                                            <V66P11>
              COMPUTE SR289-DOCORIGAMT  = SR289-DOCORIGAMT * -1
              COMPUTE SR289-DOCACCTAMT  = SR289-DOCACCTAMT * -1
              MOVE 'N'                 TO WSAA-FIRST-READ               <V66P11>
              END-IF                                                    <V66P11>

              IF RBNK-RCPTREV           = SPACES           AND
                 RBNK-DOCORIGAMT        = SR289-DOCORIGAMT AND
                 RBNK-ORIGCCY           = SR289-ORIGCURR   AND
                 RBNK-SCRATE            = SR289-SCRATE     AND
                 RBNK-DOCACCTAMT        = SR289-DOCACCTAMT AND
                 RBNK-ACCTCCY           = SR289-ACCTCCY
                 PERFORM 3622-WRITE-REVERSED-RECEIPT
                 MOVE ENDP             TO RBNK-STATUZ
              END-IF
           ELSE
              IF RBNK-RCPTREV           = SPACES           AND
                 RBNK-CHQNUM            = SR289-CHEQNO
                 PERFORM 3622-WRITE-REVERSED-RECEIPT
                 MOVE ENDP             TO RBNK-STATUZ
              END-IF
           END-IF.
      *
       3621-READ-NEXT.
           MOVE NEXTR                  TO RBNK-FUNCTION.
       3621-EXIT.
           EXIT.
      /
       3622-WRITE-REVERSED-RECEIPT SECTION.
      *************************************
       3622-BEGIN.
           MOVE READD                  TO RBNK-FUNCTION.
           CALL 'RBNKIO'            USING RBNK-PARAMS.

           IF RBNK-STATUZ           NOT = O-K
              MOVE RBNK-PARAMS         TO SYSR-PARAMS
              MOVE RBNK-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
           MOVE WSAA-NEW-RECEIPT       TO RBNK-RCPTREV.
           IF RCTSTATFLG                                                <V76F12>
              MOVE RCTST-CU            TO RBNK-RCPTSTAT                 <V76F12>
              MOVE SR289-CNRSNCD       TO RBNK-CNRSNCD                  <V76F12>
           END-IF.                                                      <V76F12>
      *
           MOVE WRITD                  TO RBNK-FUNCTION.
           CALL 'RBNKIO'            USING RBNK-PARAMS.

           IF RBNK-STATUZ           NOT = O-K
              MOVE RBNK-PARAMS         TO SYSR-PARAMS
              MOVE RBNK-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
       3622-EXIT.
           EXIT.
      /
       3700-PRINT-RECEIPT SECTION.
      ****************************
       3700-BEGIN.
      * If print receipt flag not = 'y' , skip printing of receipt
      **** IF T3676-IND            NOT = 'Y'    OR                      <GAPPH1>
      ****    WSAA-SCRN-STATUZ     NOT = 'INSR'                         <GAPPH1>
      ****    GO TO 3700-EXIT                                           <GAPPH1>
      **** END-IF.                                                      <GAPPH1>
                                                                        <GAPPH1>
           IF T3676-IND            NOT = 'Y' AND                        <GAPPH1>
              WSAA-SBMACTION       NOT = 'B'                            <GAPPH1>
              GO TO 3700-EXIT                                           <GAPPH1>
           END-IF.                                                      <GAPPH1>

           IF PCPRT-ENABLE
              PERFORM 3710-PC-PRINTING
           ELSE
              PERFORM 3720-ONLINE-PRINTING
              MOVE TR386-PROGDESC-01   TO WSAA-TEXT
           END-IF.
                                                                        <GAPPH1>
           GO TO 3700-EXIT                                              <GAPPH1>

           IF  WSAA-PRINT-WH-CERT       = 'Y'                           <V65F04>
               PERFORM 3750-PRINT-WHTAXCRT                              <V65F04>
           END-IF.                                                      <V65F04>
                                                                        <V65F04>
           PERFORM 3760-PRINT-VAT-DOC.                                  <V65P23>
                                                                        <V65P23>
       3700-EXIT.
            EXIT.
      /                                                                 <V76F12>
       3700A-PRINT-RECEIPT SECTION.                                     <V76F12>
      ****************************                                      <V76F12>
       3700A-BEGIN.                                                     <V76F12>
      * If print receipt flag not = 'y' , skip printing of receipt      <V76F12>
           IF T3676-IND            NOT = 'Y'    OR                      <V76F12>
              WSAA-SCRN-STATUZ     NOT = 'INSR'                         <V76F12>
              GO TO 3700A-EXIT                                          <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE SPACES                 TO LETRQST-STATUZ.               <V76F12>
           MOVE WSSP-COMPANY           TO LETRQST-REQUEST-COMPANY.      <V76F12>
           MOVE WSAA-RECEIPT-LETTERP   TO LETRQST-LETTER-TYPE.          <V76F12>
           MOVE WSSP-TRANDATE          TO LETRQST-LETTER-REQUEST-DATE.  <V76F12>
           MOVE WSSP-FSUCO             TO LETRQST-CLNTCOY.              <V76F12>
           MOVE WSSP-PAYESEL           TO LETRQST-CLNTNUM.              <V76F12>
           MOVE PRFX-CASH              TO LETRQST-OTHER-KEYS.           <V76F12>
           MOVE SPACES                 TO LETRQST-SERVUNIT.             <V76F12>
           MOVE WSSP-BRANCH            TO LETRQST-BRANCH.               <V76F12>
           MOVE WSAA-RECEIPT-PFX       TO LETRQST-RDOCPFX.              <V76F12>
           MOVE WSAA-RECEIPT-COY       TO LETRQST-RDOCCOY.              <V76F12>
           MOVE WSAA-NEW-RECEIPT       TO LETRQST-RDOCNUM.              <V76F12>
           MOVE ZEROES                 TO LETRQST-TRANNO.               <V76F12>
           MOVE WSKY-BATC-BATCTRCDE    TO LETRQST-TRCDE.                <V76F12>
                                                                        <V76F12>
           MOVE 'ADD'                  TO LETRQST-FUNCTION.             <V76F12>
           CALL 'LETRQST'           USING LETRQST-PARAMS.               <V76F12>
                                                                        <V76F12>
           IF  LETRQST-STATUZ       NOT = O-K                           <V76F12>
               MOVE LETRQST-STATUZ     TO SYSR-STATUZ                   <V76F12>
               PERFORM 600-FATAL-ERROR                                  <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
       3700A-REWRT.                                                     <V76F12>
           INITIALIZE                     RCPT-DATA-KEY.                <V76F12>
           MOVE PRFX-CASH              TO RCPT-RDOCPFX.                 <V76F12>
           MOVE WSSP-COMPANY           TO RCPT-RDOCCOY.                 <V76F12>
           MOVE WSAA-RECEIPT           TO RCPT-RDOCNUM.                 <V76F12>
           MOVE ZEROES                 TO RCPT-SEQNBR.                  <V76F12>
           MOVE READH                  TO RCPT-FUNCTION.                <V76F12>
           PERFORM C100-RCPTIO.                                         <V76F12>
                                                                        <V76F12>
           MOVE  'OL'                  TO RCPT-PRTFLG.                  <V76F12>
                                                                        <V76F12>
           MOVE REWRT                  TO RCPT-FUNCTION.                <V76F12>
           PERFORM C100-RCPTIO.                                         <V76F12>
                                                                        <V76F12>
      *--Update RBNK status.                                            <V76F12>
           PERFORM 3714-REWRT-RBNK.                                     <V76F12>
                                                                        <V76F12>
       3700A-EXIT.                                                      <V76F12>
            EXIT.                                                       <V76F12>
      /
       3710-PC-PRINTING SECTION.
      **************************
       3710-BEGIN.
           PERFORM 3711-ADD-LETC                                        <PCPRT2>
                                                                        <PCPRT2>
      **** IF WSSP-FLAG                = 'C'                            <PCPRT2>
      ****    PERFORM 3711-ADD-LETC                                     <PCPRT2>
      ****    MOVE BEGN                 TO WSAA-FUNCTION                <PCPRT2>
      ****    MOVE 'R'                  TO WSAA-PCP-MODE                <PCPRT2>
      ****    MOVE LETRQST-LETC-RRN     TO WSAA-LETC-RRN                <PCPRT2>
      ****    MOVE T2634-SUBRNAME (02)  TO WSAA-FORMAT-FOR              <PCPRT2>
      **** ELSE                                                         <PCPRT2>
      ****    PERFORM 3712-RETRV-LETC                                   <PCPRT2>
      ****    IF LETCUTI-STATUZ        = ENDP                           <PCPRT2>
      ****       PERFORM 3711-ADD-LETC                                  <PCPRT2>
      ****       MOVE BEGN                TO WSAA-FUNCTION              <PCPRT2>
      ****       MOVE 'R'                 TO WSAA-PCP-MODE              <PCPRT2>
      ****       MOVE LETRQST-LETC-RRN    TO WSAA-LETC-RRN              <PCPRT2>
      ****       MOVE T2634-SUBRNAME (02) TO WSAA-FORMAT-FOR            <PCPRT2>
      ****    ELSE                                                      <PCPRT2>
      ****       MOVE RETRV               TO WSAA-FUNCTION      <PCPPRT><PCPRT2>
      ****       MOVE BEGN                TO WSAA-FUNCTION      <PCPPRT><PCPRT2>
      ****       MOVE 'R'                 TO WSAA-PCP-MODE              <PCPRT2>
      ****       MOVE LETCUTI-RRN         TO WSAA-LETC-RRN              <PCPRT2>
      ****       MOVE T2634-SUBRNAME (02) TO WSAA-FORMAT-FOR            <PCPRT2>
      ****    END-IF                                                    <PCPRT2>
      **** END-IF.                                                      <PCPRT2>
      ****                                                              <PCPRT2>
      **** CALL 'PCP400'            USING WSAA-FUNCTION                 <PCPRT2>
      ****                                WSAA-STATUZ                   <PCPRT2>
      ****                                WSAA-PCP-MODE                 <PCPRT2>
      ****                                WSAA-LETC-RRN                 <PCPRT2>
      ****                                WSAA-FORMAT-FOR.              <PCPRT2>
      ****                                                              <PCPRT2>
      **** IF WSAA-STATUZ           NOT = O-K                           <PCPRT2>
      ****    MOVE WSAA-STATUZ         TO SYSR-STATUZ                   <PCPRT2>
      ****    PERFORM 600-FATAL-ERROR                                   <PCPRT2>
      **** END-IF.                                                      <PCPRT2>

           IF WSAA-SBMACTION           = 'M'                            <CS016>
              GO TO 3710-EXIT                                           <CS016>
           END-IF.                                                      <CS016>
                                                                        <CS016>
           PERFORM 3713-REWRT-RTRN.
           PERFORM 3714-REWRT-RBNK.                                     <V76F12>
       3710-EXIT.
            EXIT.
      /
       3711-ADD-LETC SECTION.
      ***********************
       3711-BEGIN.
           MOVE SPACES                 TO LETRQST-STATUZ.
           MOVE WSSP-COMPANY           TO LETRQST-REQUEST-COMPANY.
           MOVE WSAA-RECEIPT-LETTER    TO LETRQST-LETTER-TYPE.
           MOVE WSSP-TRANDATE          TO LETRQST-LETTER-REQUEST-DATE.
           MOVE WSSP-FSUCO             TO LETRQST-CLNTCOY.
           MOVE WSSP-PAYESEL           TO LETRQST-CLNTNUM.
      **** MOVE SPACES                 TO LETRQST-OTHER-KEYS.           <PCPRT2>
           MOVE PRFX-CASH              TO LETRQST-OTHER-KEYS.           <PCPRT2>
           MOVE SPACES                 TO LETRQST-SERVUNIT.
           MOVE WSSP-BRANCH            TO LETRQST-BRANCH.
           MOVE WSAA-RECEIPT-PFX       TO LETRQST-RDOCPFX.
           MOVE WSAA-RECEIPT-COY       TO LETRQST-RDOCCOY.
           MOVE WSAA-NEW-RECEIPT       TO LETRQST-RDOCNUM.
           MOVE ZEROES                 TO LETRQST-TRANNO.
           MOVE WSKY-BATC-BATCTRCDE    TO LETRQST-TRCDE.                <PCPRT2>
           IF WSAA-SBMACTION           = 'M'                            <CS016>
             MOVE WSAA-TRANCODE-CUR    TO LETRQST-TRCDE                 <CS016>
           END-IF.                                                      <CS016>

           MOVE 'ADD'                  TO LETRQST-FUNCTION.
           CALL 'LETRQST'           USING LETRQST-PARAMS.

           IF  LETRQST-STATUZ       NOT = O-K
               MOVE LETRQST-STATUZ     TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.

       3711-EXIT.
            EXIT.
      /
      *3712-RETRV-LETC SECTION.                                         <PCPRT2>
      *************************                                         <PCPRT2>
      *3712-BEGIN.                                                      <PCPRT2>
      **** MOVE WSAA-RECEIPT-PFX       TO LETCUTI-RDOCPFX.              <PCPRT2>
      **** MOVE WSAA-RECEIPT-COY       TO LETCUTI-RDOCCOY.              <PCPRT2>
      **** MOVE WSAA-NEW-RECEIPT       TO LETCUTI-RDOCNUM.              <PCPRT2>
      **** MOVE ZEROES                 TO LETCUTI-TRANNO.               <PCPRT2>
      **** MOVE WSSP-PAYESEL           TO LETCUTI-CLNTNUM.              <PCPRT2>
      **** MOVE WSAA-RECEIPT-LETTER    TO LETCUTI-LETTER-TYPE.          <PCPRT2>
      **** MOVE 99999                  TO LETCUTI-LETTER-SEQNO.         <PCPRT2>
      **** MOVE BEGN                   TO LETCUTI-FUNCTION.             <PCPRT2>
      ****                                                              <PCPRT2>
      **** CALL 'LETCUTIIO'         USING LETCUTI-PARAMS.               <PCPRT2>
      ****                                                              <PCPRT2>
      **** IF  LETCUTI-STATUZ       NOT = O-K AND ENDP                  <PCPRT2>
      ****     MOVE LETCUTI-STATUZ     TO SYSR-STATUZ                   <PCPRT2>
      ****     PERFORM 600-FATAL-ERROR                                  <PCPRT2>
      **** END-IF.                                                      <PCPRT2>
      ****                                                              <PCPRT2>
      **** IF  LETCUTI-STATUZ          = ENDP                           <PCPRT2>
      **** OR  WSAA-RECEIPT-PFX    NOT = LETCUTI-RDOCPFX                <PCPRT2>
      **** OR  WSAA-RECEIPT-COY    NOT = LETCUTI-RDOCCOY                <PCPRT2>
      **** OR  WSAA-NEW-RECEIPT    NOT = LETCUTI-RDOCNUM                <PCPRT2>
      **** OR  ZEROES              NOT = LETCUTI-TRANNO                 <PCPRT2>
      **** OR  WSSP-PAYESEL        NOT = LETCUTI-CLNTNUM                <PCPRT2>
      **** OR  WSAA-RECEIPT-LETTER NOT = LETCUTI-LETTER-TYPE            <PCPRT2>
      ****     MOVE ENDP              TO LETCUTI-STATUZ                 <PCPRT2>
      **** END-IF.                                                      <PCPRT2>
      ****                                                              <PCPRT2>
      *3712-EXIT.                                                       <PCPRT2>
      ****  EXIT.                                                       <PCPRT2>
      /
       3713-REWRT-RTRN SECTION.
      *************************
       3713-BEGIN.

           INITIALIZE                     ADOC-DATA-AREA.
           MOVE PRFX-CASH              TO ADOC-RTRN-RDOCPFX.
           MOVE WSSP-COMPANY           TO ADOC-RTRN-RDOCCOY.
           MOVE WSAA-NEW-RECEIPT       TO ADOC-RTRN-RDOCNUM.
           MOVE ZEROES                 TO ADOC-RTRN-TRANNO.
           MOVE ZEROES                 TO ADOC-RTRN-JRNSEQ.
           MOVE READH                  TO ADOC-FUNCTION.
           MOVE SPACES                 TO ADOC-FORMAT.

           CALL 'ADOCIO'            USING ADOC-PARAMS.

           IF  ADOC-STATUZ          NOT = O-K
               MOVE ADOC-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE  'OL'                  TO ADOC-RTRN-PRTFLG.
           MOVE  REWRT                 TO ADOC-FUNCTION.

           CALL  'ADOCIO'           USING ADOC-PARAMS.

           IF  ADOC-STATUZ            NOT = O-K
               MOVE ADOC-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.
       3713-EXIT.
            EXIT.
      /                                                                 <V76F12>
       3714-REWRT-RBNK SECTION.                                         <V76F12>
      *************************                                         <V76F12>
       3714-BEGIN.                                                      <V76F12>
           IF NOT RCTSTATFLG                                            <V76F12>
              GO TO 3714-EXIT.                                          <V76F12>
                                                                        <V76F12>
           INITIALIZE                     RBNKSEQ-DATA-AREA.            <V76F12>
           MOVE PRFX-CASH              TO RBNKSEQ-RDOCPFX.              <V76F12>
           MOVE WSSP-RDOCCOY           TO RBNKSEQ-RDOCCOY.              <V76F12>
           MOVE WSAA-RECEIPT           TO RBNKSEQ-RDOCNUM.              <V76F12>
           MOVE ZEROES                 TO RBNKSEQ-SEQNBR.               <V76F12>
                                                                        <V76F12>
           MOVE BEGN                   TO RBNKSEQ-FUNCTION.             <V76F12>
           PERFORM C300-RBNKSEQIO.                                      <V76F12>
           PERFORM UNTIL                                                <V76F12>
                 RBNKSEQ-STATUZ     NOT = O-K                           <V76F12>
              OR RBNKSEQ-RDOCPFX    NOT = PRFX-CASH                     <V76F12>
              OR RBNKSEQ-RDOCCOY    NOT = WSSP-RDOCCOY                  <V76F12>
              OR RBNKSEQ-RDOCNUM    NOT = WSAA-RECEIPT                  <V76F12>
                                                                        <V76F12>
              IF RBNKSEQ-RCPTSTAT       = RCTST-EN                      <V76F12>
                 MOVE READH            TO RBNKSEQ-FUNCTION              <V76F12>
                 PERFORM C300-RBNKSEQIO                                 <V76F12>
                                                                        <V76F12>
                 MOVE RCTST-DP         TO RBNKSEQ-RCPTSTAT              <V76F12>
                                                                        <V76F12>
                 MOVE REWRT            TO RBNKSEQ-FUNCTION              <V76F12>
                 PERFORM C300-RBNKSEQIO                                 <V76F12>
              END-IF                                                    <V76F12>
                                                                        <V76F12>
              MOVE NEXTR               TO RBNKSEQ-FUNCTION              <V76F12>
              PERFORM C300-RBNKSEQIO                                    <V76F12>
           END-PERFORM.                                                 <V76F12>
       3714-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /
       3720-ONLINE-PRINTING SECTION.
      ******************************
       3720-BEGIN.
      * Get the printing subroutine from TR272 by calling RTVPRTF
           INITIALIZE                     ATRT-ATREQ-REC.
           MOVE WSSP-COMPANY           TO RTVP-COMPANY.
           MOVE SPACES                 TO RTVP-ONLN-IDENTIFIER
                                          RTVP-CNTTYP.
      **** MOVE WSAA-IDENTIFIER        TO RTVP-BATC-IDENTIFIER.         <FA3472>
           MOVE WSSP-LANGUAGE          TO WSAA-RTVPRTF-LANGUAGE.        <FA3472>
           MOVE 'RC'                   TO WSAA-RTVPRTF-IDENTIFIER.      <FA3472>
           MOVE WSAA-RTVPRTF-ITEMKEY   TO RTVP-BATC-IDENTIFIER.         <FA3472>
           CALL 'RTVPRTF'           USING RTVP-RTVPRTF-REC.

           IF RTVP-STATUZ           NOT = O-K
              GO TO 3720-EXIT
           END-IF.

           MOVE RTVP-PRINT-SUBR        TO ATRT-MODULE.

           IF SBM-INQUIRY
              MOVE WSAA-SAVE-BATCKEY   TO ATRT-BATCH-KEY
           ELSE
              MOVE WSSP-BATCHKEY       TO ATRT-BATCH-KEY
           END-IF.
                                                                        <CS016>
           IF WSAA-SBMACTION           = 'M'                            <CS016>
              MOVE WSSP-BATCKEY        TO ATRT-BATCH-KEY                <CS016>
           END-IF.                                                      <CS016>

           MOVE WSAA-PROG              TO ATRT-REQ-PROG.
           MOVE VRCM-USER              TO ATRT-REQ-USER.
           MOVE VRCM-TERMID            TO ATRT-REQ-TERM.
           MOVE WSAA-TODAY             TO ATRT-REQ-DATE.
           MOVE WSAA-TIME              TO ATRT-REQ-TIME.
           MOVE WSSP-LANGUAGE          TO ATRT-LANGUAGE.
           MOVE WSAA-DOCTKEY           TO ATRT-PRIMARY-KEY.
           MOVE SPACES                 TO ATRT-TRANS-AREA.
           MOVE WSSP-RLDGKEY           TO ATRT-TRANS-AREA.

           CALL 'ATREQ'             USING ATRT-ATREQ-REC.

           IF ATRT-STATUZ           NOT = O-K
              MOVE ATRT-STATUZ         TO SYSR-STATUZ
              MOVE '2'                 TO SYSR-SYSERR-TYPE
              PERFORM 600-FATAL-ERROR
           END-IF.
       3720-EXIT.
            EXIT.
      /
       3750-PRINT-WHTAXCRT SECTION.                                     <V65F04>
      *****************************                                     <V65F04>
       3750-START.                                                      <V65F04>
      * Get the printing subroutine from TR272 by calling RTVPRTF       <V65F04>
                                                                        <V65F04>
           INITIALIZE                     RTVP-RTVPRTF-REC.             <V65F04>
           MOVE WSSP-COMPANY           TO RTVP-COMPANY.                 <V65F04>
      **** MOVE WSAA-WHTAXCRT          TO RTVP-BATC-IDENTIFIER. <FA3472><V65F04>
           MOVE WSSP-LANGUAGE          TO WSAA-RTVPRTF-LANGUAGE.        <FA3472>
           MOVE 'WH'                   TO WSAA-RTVPRTF-IDENTIFIER.      <FA3472>
           MOVE WSAA-RTVPRTF-ITEMKEY   TO RTVP-BATC-IDENTIFIER.         <FA3472>
                                                                        <V65F04>
           CALL 'RTVPRTF'           USING RTVP-RTVPRTF-REC.             <V65F04>
           IF RTVP-STATUZ           NOT = O-K                           <V65F04>
              GO TO 3750-EXIT                                           <V65F04>
           END-IF.                                                      <V65F04>
                                                                        <V65F04>
           INITIALIZE                     ATRT-ATREQ-REC.               <V65F04>
           MOVE RTVP-PRINT-SUBR        TO ATRT-MODULE.                  <V65F04>
           MOVE WSAA-PROG              TO ATRT-REQ-PROG.                <V65F04>
           MOVE WSAA-TODAY             TO ATRT-REQ-DATE.                <V65F04>
           MOVE WSAA-TIME              TO ATRT-REQ-TIME.                <V65F04>
           MOVE WSAA-DOCTKEY           TO ATRT-PRIMARY-KEY.             <V65F04>
           MOVE VRCM-USER              TO ATRT-REQ-USER.                <V65F04>
           MOVE VRCM-TERMID            TO ATRT-REQ-TERM.                <V65F04>
           MOVE WSSP-LANGUAGE          TO ATRT-LANGUAGE.                <V65F04>
                                                                        <V65F04>
           INITIALIZE                     WSAA-TRANS-AREA.              <V65F04>
           MOVE WSSP-FSUCO             TO WSAA-TRANS-FSUCO.             <V65F04>
           MOVE WSAA-TRANS-AREA        TO ATRT-TRANS-AREA.              <V65F04>
                                                                        <V65F04>
           IF SBM-INQUIRY                                               <V65F04>
              MOVE WSAA-SAVE-BATCKEY   TO ATRT-BATCH-KEY                <V65F04>
           ELSE                                                         <V65F04>
              MOVE WSSP-BATCHKEY       TO ATRT-BATCH-KEY                <V65F04>
           END-IF.                                                      <V65F04>
                                                                        <V65F04>
           CALL 'ATREQ'             USING ATRT-ATREQ-REC.               <V65F04>
           IF  ATRT-STATUZ          NOT = O-K                           <V65F04>
               MOVE ATRT-STATUZ        TO SYSR-STATUZ                   <V65F04>
               MOVE '2'                TO SYSR-SYSERR-TYPE              <V65F04>
               PERFORM 600-FATAL-ERROR                                  <V65F04>
           END-IF.                                                      <V65F04>
                                                                        <V65F04>
       3750-EXIT.                                                       <V65F04>
            EXIT.                                                       <V65F04>
                                                                        <V65F04>
       3760-PRINT-VAT-DOC SECTION.                                      <V65P23>
      *****************************                                     <V65P23>
       3761-START.                                                      <V65P23>
                                                                        <V65P23>
           IF WSAA-VATS-CALL        NOT = 'Y'                           <V65P23>
              GO TO 3769-EXIT                                           <V65P23>
           END-IF.                                                      <V65P23>
                                                                        <V65P23>
           MOVE PRFX-CASH              TO CSHD-DOCT-PREFIX.             <V65P23>
           MOVE WSSP-COMPANY           TO CSHD-DOCT-COMPANY.            <V65P23>
           MOVE S2610-DOCNO            TO CSHD-DOCT-NUMBER.             <V65P23>
           MOVE 'PRINT'                TO CSHD-FUNCTION.                <V65P23>
                                                                        <V65P23>
           CALL WSAA-VAT-ON-SALES   USING CSHD-CASHED-REC.              <V65P23>
           IF  CSHD-STATUZ          NOT = O-K                           <V65P23>
               MOVE CSHD-CASHED-REC    TO SYSR-PARAMS                   <V65P23>
               MOVE CSHD-STATUZ        TO SYSR-STATUZ                   <V65P23>
               PERFORM 600-FATAL-ERROR                                  <V65P23>
           END-IF.                                                      <V65P23>
                                                                        <V65P23>
       3769-EXIT.                                                       <V65P23>
            EXIT.                                                       <V65P23>
                                                                        <V65P23>
       3800A-PREPRINTED-RECEIPT SECTION.                                <RC002>
      **********************************                                <RC002>
       3810A-START.                                                     <RC002>
      * In case of Receipt Cancellation:                                <RC002>
      * Read the ZPPRENQ with Receipt number original                   <RC002>
      * Update the record with Validflag = 2                            <RC002>
            INITIALIZE                     ZPPRENQ-PARAMS.              <RC002>
            MOVE PRFX-CASH              TO ZPPRENQ-PREFIX.              <RC002>
            MOVE WSSP-COMPANY           TO ZPPRENQ-COMPANY.             <RC002>
            MOVE WSSP-BANKCODE          TO ZPPRENQ-BANKCODE.            <RC002>
            MOVE WSSP-RDOCNUM           TO ZPPRENQ-RECEIPT.             <RC002>
            MOVE  '1'                   TO ZPPRENQ-VALIDFLAG.           <RC002>
                                                                        <RC002>
            MOVE ZPPRENQREC             TO ZPPRENQ-FORMAT.              <RC002>
            MOVE BEGNH                  TO ZPPRENQ-FUNCTION.            <RC002>
                                                                        <RC002>
            CALL 'ZPPRENQIO'         USING ZPPRENQ-PARAMS.              <RC002>
                                                                        <RC002>
            IF ZPPRENQ-STATUZ            = ENDP                         <RC002>
                MOVE RLSE               TO ZPPRENQ-FUNCTION             <RC002>
                CALL 'ZPPRENQIO'     USING ZPPRENQ-PARAMS               <RC002>
                IF ZPPRENQ-STATUZ    NOT = O-K                          <RC002>
                    MOVE ZPPRENQ-STATUZ TO SYSR-STATUZ                  <RC002>
                    MOVE ZPPRENQ-PARAMS TO SYSR-PARAMS                  <RC002>
                    PERFORM 600-FATAL-ERROR                             <RC002>
                END-IF                                                  <RC002>
                GO TO 3890A-EXIT                                        <RC002>
            END-IF.                                                     <RC002>
                                                                        <RC002>
            IF ZPPRENQ-STATUZ        NOT = O-K                          <RC002>
                MOVE ZPPRENQ-STATUZ     TO SYSR-STATUZ                  <RC002>
                MOVE ZPPRENQ-PARAMS     TO SYSR-PARAMS                  <RC002>
                PERFORM 600-FATAL-ERROR                                 <RC002>
            END-IF.                                                     <RC002>
                                                                        <RC002>
            IF ZPPRENQ-STATUZ            = O-K                          <RC002>
            AND ZPPRENQ-PREFIX           = PRFX-CASH                    <RC002>
            AND ZPPRENQ-COMPANY          = WSSP-COMPANY                 <RC002>
            AND ZPPRENQ-RECEIPT          = WSSP-RDOCNUM                 <RC002>
            AND ZPPRENQ-BANKCODE         = WSSP-BANKCODE                <RC002>
            AND ZPPRENQ-VALIDFLAG        =  '1'                         <RC002>
      ***       MOVE '2'                TO ZPPRENQ-VALIDFLAG            <RC002>
                MOVE '3'                TO ZPPRENQ-VALIDFLAG            <RC002>
                MOVE REWRT              TO ZPPRENQ-FUNCTION             <RC002>
                CALL 'ZPPRENQIO'     USING ZPPRENQ-PARAMS               <RC002>
                                                                        <RC002>
                IF ZPPRENQ-STATUZ    NOT = O-K                          <RC002>
                    MOVE ZPPRENQ-STATUZ      TO SYSR-STATUZ             <RC002>
                    MOVE ZPPRENQ-PARAMS      TO SYSR-PARAMS             <RC002>
                    PERFORM 600-FATAL-ERROR                             <RC002>
                END-IF                                                  <RC002>
            END-IF.                                                     <RC002>
                                                                        <RC002>
       3890A-EXIT.                                                      <RC002>
            EXIT.                                                       <RC002>
      /                                                                 <RC002>
       3800-DELETE-PKFK SECTION.                                        <V65P23>
      *************************                                         <V62P04>
       3810-CALL-P2200UPKK.                                             <V62P04>
      *                                                                 <V62P04>
            MOVE O-K               TO PYED-STATUZ.                      <V62P04>
            MOVE 'DELET'           TO PYED-FUNCTION.                    <V62P04>
                                                                        <V62P04>
            MOVE PRFX-CASH         TO PYED-KFK-RDOCPFX.                 <V62P04>
            MOVE WSSP-COMPANY      TO PYED-KFK-RDOCCOY.                 <V62P04>
            MOVE S2610-DOCNO       TO PYED-KFK-RDOCNUM.                 <V62P04>
            MOVE SPACES            TO PYED-KFK-TRANSEQ.                 <V62P04>
            MOVE SCRN-SUBFILE-RRN  TO PYED-SUBFILE-RRN.                 <V62P04>
                                                                        <V62P04>
      ***   CALL 'P2200UPKK'    USING PYED-PAYEDTREC-REC.       <V63P28><V62P04>
            CALL WSAA-P2200UPKK USING PYED-PAYEDTREC-REC.               <V63P28>
                                                                        <V62P04>
            IF PYED-STATUZ      NOT = O-K                               <V62P04>
               MOVE PYED-STATUZ    TO SYSR-STATUZ                       <V62P04>
               MOVE 'P2200UPKK'    TO SYSR-SUBRNAME                     <V62P04>
               PERFORM 600-FATAL-ERROR                                  <V62P04>
            END-IF.                                                     <V62P04>
                                                                        <V62P04>
       3890-EXIT.                                                       <V62P04>
           EXIT.                                                        <V62P04>
      /                                                                 <V62P04>
       3900-UPDATE-KFK SECTION.                                         <V62P04>
      *************************                                         <V62P04>
       3910-CALL-P2200UPKK.                                             <V62P04>
      *                                                                 <V62P04>
            MOVE O-K               TO PYED-STATUZ.                      <V62P04>
            MOVE 'UPDAT'           TO PYED-FUNCTION.                    <V62P04>
                                                                        <V62P04>
            MOVE PRFX-CASH         TO PYED-KFK-RDOCPFX.                 <V62P04>
            MOVE WSSP-COMPANY      TO PYED-KFK-RDOCCOY.                 <V62P04>
            MOVE S2610-DOCNO       TO PYED-KFK-RDOCNUM.                 <V62P04>
            MOVE CSHD-TRANSEQ      TO PYED-KFK-TRANSEQ.                 <V62P04>
            MOVE SCRN-SUBFILE-RRN  TO PYED-SUBFILE-RRN.                 <V62P04>
                                                                        <V62P04>
      ***   CALL 'P2200UPKK'    USING PYED-PAYEDTREC-REC.       <V63P28><V62P04>
            CALL WSAA-P2200UPKK USING PYED-PAYEDTREC-REC.               <V63P28>
                                                                        <V62P04>
            IF PYED-STATUZ      NOT = O-K                               <V62P04>
               MOVE PYED-STATUZ    TO SYSR-STATUZ                       <V62P04>
               MOVE 'P2200UPKK'    TO SYSR-SUBRNAME                     <V62P04>
               PERFORM 600-FATAL-ERROR                                  <V62P04>
            END-IF.                                                     <V62P04>
                                                                        <V62P04>
       3990-EXIT.                                                       <V62P04>
           EXIT.                                                        <V62P04>
      /                                                                 <V76F12>
       3100A-UPDATE-RCPT SECTION.                                       <V76F12>
      ***************************                                       <V76F12>
       3110A-BEGIN.                                                     <V76F12>
      *--Update 1st dissection.                                         <V76F12>
           INITIALIZE                     RCPT-DATA-KEY.                <V76F12>
           MOVE PRFX-CASH              TO RCPT-RDOCPFX.                 <V76F12>
           MOVE WSSP-COMPANY           TO RCPT-RDOCCOY.                 <V76F12>
           MOVE WSAA-RECEIPT           TO RCPT-RDOCNUM.                 <V76F12>
           MOVE ZEROES                 TO RCPT-SEQNBR.                  <V76F12>
                                                                        <V76F12>
           MOVE READH                  TO RCPT-FUNCTION.                <V76F12>
           PERFORM C200-RCPTIO.                                         <V76F12>
           IF RCPT-STATUZ               = O-K                           <V76F12>
              MOVE REWRT               TO RCPT-FUNCTION                 <V76F12>
           ELSE                                                         <V76F12>
              MOVE WRITR               TO RCPT-FUNCTION                 <V76F12>
              INITIALIZE                  RCPT-DATA-AREA                <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE ZEROES                 TO WSAA-SEQNBR.                  <V76F12>
           MOVE WSKY-BATC-BATCPFX      TO RCPT-BATCPFX.                 <V76F12>
           MOVE WSKY-BATC-BATCCOY      TO RCPT-BATCCOY.                 <V76F12>
           MOVE WSKY-BATC-BATCBRN      TO RCPT-BATCBRN.                 <V76F12>
           MOVE PRFX-CASH              TO RCPT-RDOCPFX.                 <V76F12>
           MOVE WSSP-COMPANY           TO RCPT-RDOCCOY.                 <V76F12>
           MOVE WSAA-NEW-RECEIPT       TO RCPT-RDOCNUM.                 <V76F12>
           MOVE WSAA-SEQNBR            TO RCPT-SEQNBR.                  <V76F12>
           MOVE S2610-TRANSCODE        TO RCPT-TRANCDE.                 <V76F12>
           MOVE WSSP-BANKCODE          TO RCPT-BANKCODE.                <V76F12>
           MOVE WSSP-PTCODE            TO RCPT-SACSCODE.                <V76F12>
           MOVE WSSP-DOCTYPE           TO RCPT-SACSTYP.                 <V76F12>
           MOVE S2610-TRANDSC          TO RCPT-TRANDESC.                <V76F12>
           MOVE S2610-DOCORIGAMT       TO RCPT-ORIGAMT.                 <V76F12>
           MOVE S2610-DOCACCTAMT       TO RCPT-ACCTAMT.                 <V76F12>
                                                                        <V76F12>
           MOVE S2610-ORIGCURR         TO RCPT-ORIGCCY.                 <V76F12>
           MOVE S2610-SCRATE           TO RCPT-SCRATE.                  <V76F12>
           MOVE WSSP-TRANDATE          TO RCPT-EFFDATE.                 <V76F12>
           MOVE ZEROES                 TO RCPT-SLRATE                   <V76F12>
                                          RCPT-CONTOT.                  <V76F12>
                                                                        <V76F12>
           IF WSSP-PTCODE               = CSRF-AG                       <V76F12>
              MOVE WSSP-PTCODE         TO RCPT-RLDGPFX                  <V76F12>
              MOVE WSSP-COMPANY        TO RCPT-RLDGCOY                  <V76F12>
              MOVE WSSP-PAYESEL        TO RCPT-RLDGACCT                 <V76F12>
           ELSE                                                         <V76F12>
              MOVE WSKY-CLNT-CLNTPFX   TO RCPT-RLDGPFX                  <V76F12>
              MOVE WSKY-CLNT-CLNTCOY   TO RCPT-RLDGCOY                  <V76F12>
              MOVE WSKY-CLNT-CLNTNUM   TO RCPT-RLDGACCT                 <V76F12>
           END-IF                                                       <V76F12>
                                                                        <V76F12>
           PERFORM C100-RCPTIO.                                         <V76F12>
                                                                        <V76F12>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9000-S2610-IO.                                       <V76F12>
           PERFORM 3120A-LOOP-SUBFILE                                   <V76F12>
                                    UNTIL SCRN-STATUZ = ENDP.           <V76F12>
       3190A-EXIT.                                                      <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       3110A-DELETE-RCPT SECTION.                                       <V76F12>
      ***************************                                       <V76F12>
       3111A-DELETE.                                                    <V76F12>
           INITIALIZE                     RCPT-DATA-KEY.                <V76F12>
           MOVE WSSP-RDOCPFX           TO RCPT-RDOCPFX.                 <V76F12>
           MOVE WSSP-RDOCCOY           TO RCPT-RDOCCOY.                 <V76F12>
           MOVE WSSP-RDOCNUM           TO RCPT-RDOCNUM.                 <V76F12>
           MOVE ZEROES                 TO RCPT-SEQNBR.                  <V76F12>
           MOVE BEGN                   TO RCPT-FUNCTION.                <V76F12>
                                                                        <V76F12>
       3112A-CALL-RCPT.                                                 <V76F12>
           PERFORM C200-RCPTIO.                                         <V76F12>
                                                                        <V76F12>
           IF RCPT-STATUZ               = ENDP                          <V76F12>
           OR RCPT-RDOCPFX          NOT = WSSP-RDOCPFX                  <V76F12>
           OR RCPT-RDOCCOY          NOT = WSSP-RDOCCOY                  <V76F12>
           OR RCPT-RDOCNUM          NOT = WSSP-RDOCNUM                  <V76F12>
              GO TO 3119A-EXIT.                                         <V76F12>
                                                                        <V76F12>
      *--If the document has been print, do not delete the header       <V76F12>
      *--record, instead re-write.                                      <V76F12>
           IF  RCPT-SEQNBR              = ZEROES                        <V76F12>
           AND RCPT-PRTFLG          NOT = SPACES                        <V76F12>
               GO TO 3118A-NEXTR.                                       <V76F12>
                                                                        <V76F12>
           MOVE READH                  TO RCPT-FUNCTION.                <V76F12>
           PERFORM C100-RCPTIO.                                         <V76F12>
                                                                        <V76F12>
           MOVE DELET                  TO RCPT-FUNCTION.                <V76F12>
           PERFORM C100-RCPTIO.                                         <V76F12>
                                                                        <V76F12>
       3118A-NEXTR.                                                     <V76F12>
           MOVE NEXTR                  TO RCPT-FUNCTION.                <V76F12>
           GO TO 3112A-CALL-RCPT.                                       <V76F12>
                                                                        <V76F12>
       3119A-EXIT.                                                      <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       3120A-LOOP-SUBFILE SECTION.                                      <V76F12>
      ****************************                                      <V76F12>
       3121A-BEGIN.                                                     <V76F12>
           ADD  1                      TO WSAA-SEQNBR.                  <V76F12>
                                                                        <V76F12>
           IF S2610-SACSCODE           = SPACES                         <V76F12>
              GO TO 3122A-READ-NEXT-SUBFILE.                            <V76F12>
                                                                        <V76F12>
           INITIALIZE                     RCPT-DATA-AREA.               <V76F12>
           MOVE WSKY-BATC-BATCPFX      TO RCPT-BATCPFX.                 <V76F12>
           MOVE WSKY-BATC-BATCCOY      TO RCPT-BATCCOY.                 <V76F12>
           MOVE WSKY-BATC-BATCBRN      TO RCPT-BATCBRN.                 <V76F12>
           MOVE PRFX-CASH              TO RCPT-RDOCPFX.                 <V76F12>
           MOVE WSSP-COMPANY           TO RCPT-RDOCCOY.                 <V76F12>
           MOVE WSAA-NEW-RECEIPT       TO RCPT-RDOCNUM.                 <V76F12>
           MOVE WSAA-SEQNBR            TO RCPT-SEQNBR.                  <V76F12>
           MOVE WSSP-BANKCODE          TO RCPT-BANKCODE.                <V76F12>
           MOVE S2610-TRANSCODE        TO RCPT-TRANCDE.                 <V76F12>
           MOVE S2610-SACSCODE         TO RCPT-SACSCODE.                <V76F12>
           MOVE S2610-SACSTYPW         TO RCPT-SACSTYP.                 <V76F12>
           MOVE S2610-TRANDESC         TO RCPT-TRANDESC.                <V76F12>
           MOVE S2610-ORIGAMT          TO RCPT-ORIGAMT.                 <V76F12>
           MOVE S2610-ACCTAMT          TO RCPT-ACCTAMT.                 <V76F12>
           MOVE S2610-BCURR            TO RCPT-ORIGCCY.                 <V76F12>
           MOVE S2610-SLRATE           TO RCPT-SLRATE.                  <V76F12>
           MOVE S2610-SCRATE           TO RCPT-SCRATE.                  <V76F12>
           MOVE S2610-ACCPFX           TO RCPT-ACCPFX.                  <V76F12>
           MOVE S2610-ACCCOY           TO RCPT-ACCCOY.                  <V76F12>
           MOVE S2610-ACCNUM           TO RCPT-ACCNUM.                  <V76F12>
           MOVE S2610-DISACC           TO RCPT-DISACC.                  <V76F12>
           MOVE WSSP-TRANDATE          TO RCPT-EFFDATE.                 <V76F12>
           MOVE S2610-SOMEM            TO RCPT-SOMEM.                   <V76F12>
           MOVE S2610-EXTRA            TO RCPT-EXTRA.                   <V76F12>
           MOVE SPACES                 TO RCPT-PRTFLG.                  <V76F12>
                                                                        <V76F12>
           MOVE S2610-TRANKEY          TO WSAA-TRANKEY.                 <V76F12>
           MOVE WSAA-TRANPFX           TO RCPT-RLDGPFX.                 <V76F12>
           MOVE WSAA-TRANCOY           TO RCPT-RLDGCOY.                 <V76F12>
           MOVE WSAA-ENTITY            TO RCPT-RLDGACCT.                <V76F12>
           MOVE S2610-GENLKEY          TO RCPT-TRANKEY.                 <V76F12>
           MOVE S2610-SIGN             TO RCPT-GLSIGN.                  <V76F12>
           MOVE S2610-CNTTOT           TO RCPT-CONTOT.                  <V76F12>
           MOVE S2610-CHDRKEY          TO RCPT-CHDRKEY.                 <V76F12>
           MOVE S2610-BRANCH           TO RCPT-BRANCH.                  <V76F12>
                                                                        <V76F12>
           MOVE WRITR                  TO RCPT-FUNCTION.                <V76F12>
           PERFORM C100-RCPTIO.                                         <V76F12>
                                                                        <V76F12>
       3122A-READ-NEXT-SUBFILE.                                         <V76F12>
           MOVE SRDN                   TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9000-S2610-IO.                                       <V76F12>
                                                                        <V76F12>
       3129A-EXIT.                                                      <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       3200A-UPDATE-RBNKSEQ SECTION.                                    <V76F12>
      ******************************                                    <V76F12>
       3210A-BEGIN.                                                     <V76F12>
                                                                        <V76F12>
           MOVE SCRN-SUBFILE-RRN       TO WSAA-SFL-RRN.                 <V76F12>
           MOVE SCRN-SUBFILE-END       TO WSAA-SFL-END.                 <V76F12>
           MOVE WSAA-SR289-SFL-END     TO SCRN-SUBFILE-END.             <V76F12>
           MOVE SCRN-SCRNAME           TO WSAA-SCRN-SCRNAME.            <V76F12>
                                                                        <V76F12>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9100-SR289-IO.                                       <V76F12>
                                                                        <V76F12>
           PERFORM 3210A-LOOP-SR289-SUBFILE                             <V76F12>
                                    UNTIL SCRN-STATUZ = ENDP            <V76F12>
                                                                        <V76F12>
           MOVE WSAA-SCRN-SCRNAME      TO SCRN-SCRNAME.                 <V76F12>
           MOVE WSAA-SFL-RRN           TO SCRN-SUBFILE-RRN.             <V76F12>
           MOVE WSAA-SFL-END           TO SCRN-SUBFILE-END.             <V76F12>
       3290A-EXIT.                                                      <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       3210A-LOOP-SR289-SUBFILE SECTION.                                <V76F12>
      **********************************                                <V76F12>
       3211A-BEGIN.                                                     <V76F12>
           IF SR289-PAYTYPE             = SPACES                        <V76F12>
              GO TO 3212A-READ-NEXT-SUBFILE                             <V76F12>
           END-IF.                                                      <V76F12>
           IF SBM-PARTIAL-CANCEL          AND                           <V76F12>
              SR289-SELECT              = SPACES                        <V76F12>
              GO TO 3212A-READ-NEXT-SUBFILE                             <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           INITIALIZE                     RBNKSEQ-DATA-AREA.            <V76F12>
           MOVE WSSP-RDOCPFX           TO RBNKSEQ-RDOCPFX.              <V76F12>
           MOVE WSSP-RDOCCOY           TO RBNKSEQ-RDOCCOY.              <V76F12>
           MOVE WSSP-RDOCNUM           TO RBNKSEQ-RDOCNUM.              <V76F12>
           MOVE SR289-SEQNBR           TO RBNKSEQ-SEQNBR.               <V76F12>
                                                                        <V76F12>
           MOVE READH                  TO RBNKSEQ-FUNCTION.             <V76F12>
           PERFORM C300-RBNKSEQIO.                                      <V76F12>
           IF RBNKSEQ-STATUZ            = O-K                           <V76F12>
              MOVE REWRT               TO RBNKSEQ-FUNCTION              <V76F12>
           ELSE                                                         <V76F12>
              MOVE WRITR               TO RBNKSEQ-FUNCTION              <V76F12>
              MOVE WSSP-RDOCPFX        TO RBNKSEQ-RDOCPFX               <V76F12>
              MOVE WSSP-RDOCCOY        TO RBNKSEQ-RDOCCOY               <V76F12>
              MOVE WSSP-RDOCNUM        TO RBNKSEQ-RDOCNUM               <V76F12>
              MOVE SR289-SEQNBR        TO RBNKSEQ-SEQNBR                <V76F12>
              MOVE SPACES              TO RBNKSEQ-MCHNTID               <V76F12>
                                          RBNKSEQ-TRMNLID               <V76F12>
                                          RBNKSEQ-CRCARDTYPE            <V76F12>
                                          RBNKSEQ-CRDTCARD              <V76F13>
                                          RBNKSEQ-CRCNAME               <V76F13>
                                          RBNKSEQ-HLRSNCD               <V76F12>
                                          RBNKSEQ-CNRSNCD               <V76F12>
                                          RBNKSEQ-INSREFNO              <V76F12>
              MOVE ZEROS               TO RBNKSEQ-CRCARDEXPY            <V76F12>
                                          RBNKSEQ-CRCARDEXPM            <V76F12>
                                          RBNKSEQ-DISHONNO              <V76F12>
                                          RBNKSEQ-CRCARDCHG             <V76F13>
              MOVE VRCM-MAX-DATE       TO RBNKSEQ-AUTHDAT               <V76F12>
                                          RBNKSEQ-REALDAT               <V76F12>
                                          RBNKSEQ-DEPDATE               <V76F12>
              MOVE WSAA-TODAY          TO RBNKSEQ-CRTDATE               <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           IF (SBM-CANCELLATION OR SBM-PARTIAL-CANCEL)                  <V76F12>
           AND RCTSTATFLG                                               <V76F12>
              MOVE RCTST-CU            TO RBNKSEQ-RCPTSTAT              <V76F12>
              MOVE SR289-CNRSNCD       TO RBNKSEQ-CNRSNCD               <V76F12>
              MOVE WSSP-RDOCNUM        TO RBNKSEQ-RCPTREV               <V76F12>
              GO TO 3212A-UPDATE                                        <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE SR289-CHEQNO           TO RBNKSEQ-CHQNUM.               <V76F12>
           MOVE SR289-TCHQDATE         TO RBNKSEQ-TCHQDATE.             <V76F12>
           MOVE SR289-DEPDATE          TO RBNKSEQ-DEPDATE.              <V76F12>
           MOVE SR289-BANKDESC-01      TO RBNKSEQ-BANKDESC01.           <V76F12>
           MOVE SR289-BANKDESC-02      TO RBNKSEQ-BANKDESC02.           <V76F12>
           MOVE SR289-BANKDESC-03      TO RBNKSEQ-BANKDESC03.           <V76F12>
           MOVE SR289-ZCHQTYP          TO RBNKSEQ-ZCHQTYP.              <V76F12>
           MOVE SR289-PAYTYPE          TO RBNKSEQ-PAYTYPE.              <V76F12>
           MOVE SR289-BANKKEY          TO RBNKSEQ-BANKKEY.              <V76F12>
           MOVE SR289-DOCORIGAMT       TO RBNKSEQ-DOCORIGAMT.           <V76F12>
           MOVE SR289-ORIGCURR         TO RBNKSEQ-ORIGCCY.              <V76F12>
           MOVE SR289-SCRATE           TO RBNKSEQ-SCRATE.               <V76F12>
           MOVE SR289-DOCACCTAMT       TO RBNKSEQ-DOCACCTAMT.           <V76F12>
           MOVE SR289-ACCTCCY          TO RBNKSEQ-ACCTCCY.              <V76F12>
           MOVE WSAA-MARRYFLAG         TO RBNKSEQ-MARRYFLAG.            <V76F12>
           MOVE SR289-RCPTSTAT         TO RBNKSEQ-RCPTSTAT.             <V76F12>
           MOVE SR289-CRCARDMOD        TO RBNKSEQ-CRCARDMOD.            <V76F12>
           MOVE WSSP-CHEQ-PRESFLAG     TO RBNKSEQ-POSTDTEFLG.           <V76F12>
           MOVE WSAA-TODAY             TO RBNKSEQ-TRANSACTION-DATE.     <V76F12>
           MOVE WSAA-TIME              TO RBNKSEQ-TRANSACTION-TIME.     <V76F12>
           MOVE VRCM-USER              TO RBNKSEQ-USER.                 <V76F12>
                                                                        <V76F12>
       3212A-UPDATE.                                                    <V76F12>
           PERFORM C300-RBNKSEQIO.                                      <V76F12>
                                                                        <V76F12>
       3212A-READ-NEXT-SUBFILE.                                         <V76F12>
           MOVE SRDN                   TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9100-SR289-IO.                                       <V76F12>
       3219A-EXIT.                                                      <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       3300A-OPEN-BATC-KEY SECTION.                                     <V76F12>
      *****************************                                     <V76F12>
       3300A-BEGIN.                                                     <V76F12>
      *--Due to the introduction of Post-Dated Receipt, creation of     <V76F12>
      *--BATC-KEY should be done only if the receipt is confirm.        <V76F12>
      *--Which means that it is posted in RTRN.                         <V76F12>
                                                                        <V76F12>
      *--Example: If the Receipt is postdated and it was cancel,        <V76F12>
      *--no batch should be created in BATC as no posting is done in    <V76F12>
      *--RTRN.                                                          <V76F12>
                                                                        <V76F12>
      *--Set-up in table T1690 BATCH-REQUIRED should be 'N'.  This      <V76F12>
      *--is applicable for all action.  As all updating is done here.   <V76F12>
                                                                        <V76F12>
           MOVE 'AUTO'                 TO BATD-FUNCTION.                <V76F12>
           MOVE VRCM-TRANID            TO BATD-TRANID.                  <V76F12>
           MOVE WSSP-BATCHKEY          TO BATD-BATCHKEY.                <V76F12>
           CALL 'BATCDOR' USING BATD-BATCDOR-REC.                       <V76F12>
           IF BATD-STATUZ              NOT = O-K                        <V76F12>
              MOVE BATD-STATUZ         TO SYSR-STATUZ                   <V76F12>
              PERFORM 600-FATAL-ERROR                                   <V76F12>
           END-IF.                                                      <V76F12>
           MOVE BATD-BATCHKEY          TO WSSP-BATCHKEY.                <V76F12>
       3300A-EXIT.                                                      <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       3400A-UPDATE-DISSECTION SECTION.                                 <V76F12>
      ****************                                                  <V76F12>
       3400A-BEGIN.                                                     <V76F12>
           MOVE SSTRT                  TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9000-S2610-IO.                                       <V76F12>
           PERFORM 3410A-LOOP-SUBFILE                                   <V76F12>
                                       UNTIL SCRN-STATUZ = ENDP.        <V76F12>
       3400A-EXIT.                                                      <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       3410A-LOOP-SUBFILE SECTION.                                      <V76F12>
      ****************************                                      <V76F12>
       3410A-BEGIN.                                                     <V76F12>
      *---KFK IS APPLICABLE ONLY FOR CLAIMS                             <V76F12>
           IF S2610-SACSCODE           = 'CL'                           <V76F12>
           AND WSAA-P2200UPKK-CALL     = 'Y'                            <V76F12>
           AND (SBM-CANCELLATION OR SBM-PARTIAL-CANCEL)                 <V76F12>
              PERFORM 3800-DELETE-PKFK                                  <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
       3410A-READ-NEXT-SUBFILE.                                         <V76F12>
           MOVE SRDN                   TO SCRN-FUNCTION.                <V76F12>
           PERFORM 9000-S2610-IO.                                       <V76F12>
       3410A-EXIT.                                                      <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.

           MOVE SCRN-SUBFILE-RRN       TO WSAA-SFL-RRN.
           MOVE SCRN-SUBFILE-END       TO WSAA-SFL-END.

      * Check for function key switching.

           MOVE 'N'                    TO WSAA-FOUND-FKEY.
           PERFORM 4100-FKEY-SWITCHING.
           IF FOUND-FKEY
              GO TO 4010-STCK-OPTS
           END-IF.
      *
           MOVE O-K                    TO SCRN-STATUZ.
           MOVE 'N'                    TO WSAA-FOUND-SELECTION.
           IF WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) = '*'                     <FA2088>
              MOVE SRDN                TO SCRN-FUNCTION                 <FA2088>
             ELSE                                                       <FA2088>
           MOVE SSTRT                  TO SCRN-FUNCTION.

           PERFORM 4200-LINE-SELS      UNTIL SCRN-STATUZ = ENDP
                                          OR FOUND-SELECTION.

           IF FOUND-SELECTION
              GO TO 4010-STCK-OPTS
           END-IF.
                                                                        <V66P11>
           PERFORM 4300-CHECK-BOX-SWITCHING.                            <V66P11>
                                                                        <V66P11>
           IF  FOUND-CHK-BOX                                            <V66P11>
               GO TO 4010-STCK-OPTS                                     <V66P11>
           END-IF.                                                      <V66P11>

       4010-STCK-OPTS.

           MOVE 'STCK'                 TO OPTS-FUNCTION.
           CALL 'OPTSWCH'           USING OPTSWCH-REC
                                          WSSP-SEC-PROGS
                                          WSSP-SEC-ACTNS
                                          WSSP-PROGRAM-PTR
                                          WSSP-FLAG.

           IF OPTS-STATUZ          NOT = O-K AND ENDP
              MOVE 'STCK'              TO SYSR-FUNCTION
              MOVE OPTS-STATUZ         TO SYSR-DBIO-STATUZ
                                          SYSR-STATUZ
              MOVE 'OPTSWCH'           TO SYSR-IOMOD
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF OPTS-STATUZ              = ENDP
              MOVE SCRN-SCRNAME        TO WSSP-NEXTPROG
              IF WSAA-SCRN-STATUZ      = O-K
      ***        MOVE WSAA-SFL-RRN     TO SCRN-SUBFILE-RRN              <R2-296>
                 MOVE WSAA-SFL-END     TO SCRN-SUBFILE-END
                 MOVE WSAA-CURRENT-RRN TO SCRN-SUBFILE-RRN              <R2-296>
              END-IF
              IF  WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) = ' '                 <V66P11>
                  MOVE ZEROES                  TO WSAA-IX               <V66P11>
                  PERFORM VARYING WSAA-COUNT FROM 3 BY 1                <V66P11>
                    UNTIL WSAA-COUNT > WSAA-MAX-OPT                     <V66P11>
                       PERFORM 1111-MOVE-OPTSDSC                        <V66P11>
                  END-PERFORM                                           <V66P11>
                  PERFORM 4400-PROTECT-SUBFILE                          <V66P11>
              END-IF                                                    <V66P11>
           ELSE
              MOVE WSAA-SFL-RRN        TO WSSP-SUBFILE-RRN
              MOVE WSAA-SFL-END        TO WSSP-SUBFILE-END
              ADD 1                    TO WSSP-PROGRAM-PTR
              STRING WSAA-NEW-RECEIPT DELIMITED BY SIZE
                     WSAA-TEXT    DELIMITED BY SIZE
                INTO WSSP-CONFIRMATION-KEY
      *--The transaction code in WSSP-BATCHKEY determine the message    <V76F12>
      *--to be display when program return back to sub-menu.  This is   <V76F12>
      *--the the sub-menu transaction.                                  <V76F12>
      *--                                                               <V76F12>
      *--For modification, the transaction saved in WSSP-BATCHKEY is    <V76F12>
      *--replace by the receipt transaction code in RTRN/RCPT.          <V76F12>
      *--So when returning back to submenu return the submenu           <V76F12>
      *--transaction code save in WSSP-BATCKEY(saving is done           <V76F12>
      *-- in 3000- section of P2067)                                    <V76F12>
              IF SBM-MODIFY                                             <V76F12>
              AND WSSP-SEC-PROG(WSSP-PROGRAM-PTR)                       <V76F12>
                                        = 'COMIT'                       <V76F12>
              AND WSAA-SBMACTION        = 'K'                           <CS016>
                 MOVE WSSP-BATCKEY           TO WSSP-BATCHKEY           <V76F12>
              END-IF                                                    <V76F12>
           END-IF.
      *
       4090-EXIT.
            EXIT.
      /
       4100-FKEY-SWITCHING SECTION.
      *****************************
       4100-BEGIN.

           PERFORM VARYING WSAA-COUNT FROM 1 BY 1
                   UNTIL WSAA-COUNT > WSAA-MAX-OPT
                      OR FOUND-FKEY
              IF OPTS-TYPE(WSAA-COUNT) = 'F' AND
                 OPTS-CODE(WSAA-COUNT) = WSAA-SCRN-STATUZ
                 MOVE 'F'              TO OPTS-SEL-TYPE
                 MOVE WSAA-SCRN-STATUZ TO OPTS-SEL-CODE
                 MOVE ZEROES           TO OPTS-SEL-OPTNO
                 MOVE SPACES           TO WSAA-SCRN-STATUZ
                 MOVE 'Y'              TO WSAA-FOUND-FKEY
              END-IF
           END-PERFORM.

       4100-EXIT.
           EXIT.
      /
       4200-LINE-SELS SECTION.
      ************************
       4200-BEGIN.

           PERFORM 9000-S2610-IO.

           IF SCRN-STATUZ              = ENDP
              GO TO 4200-EXIT
           END-IF.

      ***  IF S2610-SELECT-NUMBER      = 2                              <FA2088>
      ***     GO TO 4200-NEXT                                           <FA2088>
      ***  END-IF.                                                      <FA2088>
           IF S2610-SACSCODE           = SPACES                         <R2-296>
              GO TO 4200-NEXT                                           <R2-296>
           END-IF.                                                      <R2-296>

           MOVE SCRN-SUBFILE-RRN       TO WSAA-SFL-RRN.
           MOVE SCRN-SUBFILE-END       TO WSAA-SFL-END.

           MOVE S2610-TRANNO           TO WSSP-TRANNO.
           MOVE 'L'                    TO OPTS-SEL-TYPE.

           IF S2610-INDIC           NOT = SPACES AND 'Y'
              MOVE S2610-INDIC         TO OPTS-SEL-OPTNO
      ***                                 S2610-SELECT-NUMBER           <FA2088>
           ELSE
              IF SBM-PARTIAL-CANCEL       AND                           <FA2088>
                 S2610-SELECT-NUMBER    = 2                             <FA2088>
                 NEXT SENTENCE                                          <FA2088>
                ELSE                                                    <FA2088>
              MOVE S2610-SELECT-NUMBER TO OPTS-SEL-OPTNO
           END-IF.

           MOVE SPACES                 TO OPTS-SEL-CODE.
           IF OPTS-SEL-OPTNO            = 1  AND
              (S2610-SACSCODE           = 'CL' OR 'TP' OR 'WH'          <V65P23>
                                               OR 'VS' OR 'VP' )        <V65P23>
      ****    (S2610-SACSCODE           = 'CL' OR 'TP' OR 'WH')         <V65F04>
      ****    (S2610-SACSCODE           = 'CL' OR 'TP')                 <V65F04>
              MOVE S2610-SACSCODE      TO OPTS-SEL-CODE
           END-IF.
           IF OPTS-SEL-OPTNO            = 1  AND                        <V72P02>
              (S2610-SACSTYPW           = SUBA-AGFPAY-TYPE OR           <V72P02>
                                          SUBA-CNBGPAY-TYPE OR          <V72P02>
                                          SUBA-CNCDPAY-TYPE)            <V72P02>
              MOVE 'PS'                TO OPTS-SEL-CODE                 <V72P02>
           END-IF.                                                      <V72P02>

           IF SBM-INQUIRY          AND
              S2610-SELECT-NUMBER  NOT = 1
              MOVE SPACES              TO OPTS-SEL-CODE
           END-IF.

           PERFORM VARYING WSAA-COUNT  FROM 1 BY 1
                                      UNTIL WSAA-COUNT > WSAA-MAX-OPTA  <V72P02>
              IF OPTS-TYPE(WSAA-COUNT) = 'L'                  AND
      ***        OPTS-NO  (WSAA-COUNT) = S2610-SELECT-NUMBER  AND       <FA2088>
                 OPTS-NO  (WSAA-COUNT) = OPTS-SEL-OPTNO       AND       <FA2088>
                 OPTS-CODE(WSAA-COUNT) = OPTS-SEL-CODE
                 MOVE 'X'              TO OPTS-IND(WSAA-COUNT)
                 MOVE 'Y'              TO WSAA-FOUND-SELECTION
                 MOVE 'Y'              TO S2610-INDIC
              END-IF
           END-PERFORM.

      *---FOR PARTIAL CANCEL, WE CANNOT BLANK OFF THE "2" SELECTED      <FA2088>
           IF SBM-PARTIAL-CANCEL         AND                            <FA2088>
              S2610-SELECT-NUMBER        = 2                            <FA2088>
              NEXT SENTENCE                                             <FA2088>
             ELSE                                                       <FA2088>
              MOVE ZEROS               TO S2610-SELECT-NUMBER           <FA2088>
           END-IF.                                                      <FA2088>
      ***  MOVE ZEROES                 TO S2610-SELECT-NUMBER.          <FA2088>
           IF SBM-PARTIAL-CANCEL                                        <R2-296>
              IF S2610-SELECT-NUMBER       = ZEROS AND                  <R2-296>
                 S2610-CNT             NOT = ZEROS                      <R2-296>
                 COMPUTE WSAA-DIFF         = ZEROS - S2610-ACCTAMT      <R2-296>
                 MOVE S2610-ACCTAMT       TO S2610-AMNT                 <R2-296>
                 ADD WSAA-DIFF            TO WSAA-DISS-TOTAL            <R2-296>
              END-IF                                                    <R2-296>
              COMPUTE WSAA-DIFF          = S2610-SELECT-NUMBER -        <R2-296>
                                           S2610-CNT                    <R2-296>
              MOVE S2610-SELECT-NUMBER  TO S2610-CNT                    <R2-296>
              ADD WSAA-DIFF             TO WSAA-NUM-SELECTED            <R2-296>
           END-IF.                                                      <R2-296>
           MOVE S2610-TRANSEQ          TO WSSP-TRANSEQ.                 <V65F04>
           MOVE SUPD                   TO SCRN-FUNCTION.
           PERFORM 9000-S2610-IO.

       4200-NEXT.

           MOVE SRDN                   TO SCRN-FUNCTION.

       4200-EXIT.
           EXIT.
      /
       4300-CHECK-BOX-SWITCHING SECTION.                                <V66P11>
      **********************************                                <V66P11>
       4310-START.                                                      <V66P11>
           MOVE SPACES                 TO WSAA-FOUND-CHK-BOX.           <V66P11>
                                                                        <V66P11>
           PERFORM VARYING WSAA-COUNT FROM 1 BY 1                       <V66P11>
             UNTIL WSAA-COUNT > WSAA-MAX-OPT OR FOUND-CHK-BOX           <V66P11>
               IF  OPTS-TYPE(WSAA-COUNT)  = 'X'                         <V66P11>
               AND S2610-XOPTIND          = 'X'                         <V66P11>
                   MOVE 'X'            TO OPTS-IND (WSAA-COUNT)         <V66P11>
                   MOVE 'Y'            TO WSAA-FOUND-CHK-BOX            <V66P11>
                   MOVE SPACES         TO S2610-XOPTIND                 <V66P11>
               END-IF                                                   <V66P11>
           END-PERFORM.                                                 <V66P11>
                                                                        <V66P11>
       4390-EXIT.                                                       <V66P11>
           EXIT.                                                        <V66P11>
                                                                        <V66P11>
       4400-PROTECT-SUBFILE SECTION.                                    <V66P11>
      ******************************                                    <V66P11>
       4410-START.                                                      <V66P11>
           IF  ENTER-CN-SCREEN                                          <V66P11>
               PERFORM 1500-RETRV-SAVCBOX                               <V66P11>
               IF  SVCB-IND-UPDATE      = 'Y'                           <V66P11>
                   MOVE SSTRT          TO SCRN-FUNCTION                 <V66P11>
                   PERFORM 9000-S2610-IO                                <V66P11>
                                                                        <V66P11>
                   PERFORM UNTIL SCRN-STATUZ = ENDP                     <V66P11>
                       MOVE 'Y'        TO S2610-SELNUM-OUT(PR)          <V66P11>
                       MOVE 'Y'        TO S2610-ORIGAMT-OUT(PR)         <V66P11>
                       MOVE 'Y'        TO S2610-ACCTAMT-OUT(PR)         <V66P11>
                       MOVE SUPD       TO SCRN-FUNCTION                 <V66P11>
                       PERFORM 9000-S2610-IO                            <V66P11>
                       MOVE SRDN       TO SCRN-FUNCTION                 <V66P11>
                       PERFORM 9000-S2610-IO                            <V66P11>
                   END-PERFORM                                          <V66P11>
               END-IF                                                   <V66P11>
           END-IF.                                                      <V66P11>
                                                                        <V66P11>
       4490-EXIT.                                                       <V66P11>
           EXIT.                                                        <V66P11>
      /                                                                 <PHLRMS>
       4500-UPDATE-TRRN SECTION.                                        <PHLRMS>
      **************************                                        <PHLRMS>
      *                                                                 <PHLRMS>
       4510-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     TRRN-PARAMS.                  <PHLRMS>
           MOVE WSSP-SERVUNIT          TO TRRN-RCTYPE.                  <PHLRMS>
           MOVE WSSP-COMMON-AREA(102:8)                                 <PHLRMS>
                                       TO TRRN-RCSERNUM.                <PHLRMS>
           MOVE TRRNREC                TO TRRN-FORMAT.                  <PHLRMS>
           MOVE READR                  TO TRRN-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'TRRNIO'               USING TRRN-PARAMS.               <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRN-STATUZ             NOT = O-K                        <PHLRMS>
               MOVE TRRN-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               MOVE TRRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *-- Update Valid Flag '2'                                         <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE '2'                    TO TRRN-VALIDFLAG.               <PHLRMS>
           MOVE WRITD                  TO TRRN-FUNCTION.                <PHLRMS>
           CALL 'TRRNIO'               USING TRRN-PARAMS.               <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRN-STATUZ             NOT = O-K                        <PHLRMS>
               MOVE TRRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               MOVE TRRN-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *-- Write new Valid Flag '1'                                      <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM 4600-READ-TV035.                                     <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Get new transaction No.                                      <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM 4900-GET-NEW-TRANNO.                                 <PHLRMS>
                                                                        <PHLRMS>
           MOVE '1'                    TO TRRN-VALIDFLAG.               <PHLRMS>
                                                                        <PHLRMS>
      *                                                                 <CS002>
      *--Receipt from agent transfer.                                   <CS002>
      *                                                                 <CS002>
           IF  TRRN-RCSTAT             = 'RA'                           <CS002>
               MOVE TRRN-TOPFX         TO WSAA-FRMPFX                   <CS002>
               MOVE TRRN-TOPER         TO WSAA-FROMPER                  <CS002>
               MOVE 'OF'               TO WSAA-TOPFX                    <CS002>
               MOVE TRRN-LOCALITY      TO WSAA-TOLOC                    <CS002>
      *                                                                 <CS002>
      *--  From                                                         <CS002>
      *                                                                 <CS002>
               MOVE WSAA-FRMPFX        TO TRRN-FRMPFX                   <CS002>
               MOVE WSAA-FROMPER       TO TRRN-FROMPER                  <CS002>
               MOVE SPACES             TO TRRN-FROMLOC                  <CS002>
      *                                                                 <CS002>
      *--  To                                                           <CS002>
      *                                                                 <CS002>
               MOVE WSAA-TOPFX         TO TRRN-TOPFX                    <CS002>
               MOVE WSAA-TOLOC         TO TRRN-TOLOC                    <CS002>
               MOVE SPACES             TO TRRN-TOPER                    <CS002>
           END-IF.                                                      <CS002>
      *                                                         <CS002> <PHLRMS>
      *--  Return receipt from Agent to Office                          <PHLRMS>
      *                                                                 <PHLRMS>
           IF  TRRN-RCSTAT             = 'AS'                           <PHLRMS>
               MOVE TRRN-FRMPFX        TO WSAA-TOPFX                    <PHLRMS>
               MOVE TRRN-TOPFX         TO WSAA-FRMPFX                   <PHLRMS>
               MOVE TRRN-TOPER         TO WSAA-FROMPER                  <PHLRMS>
               MOVE TRRN-FROMLOC       TO WSAA-TOLOC                    <PHLRMS>
                                                                        <PHLRMS>
               MOVE WSAA-FRMPFX        TO TRRN-FRMPFX                   <PHLRMS>
               MOVE SPACES             TO TRRN-FROMLOC                  <PHLRMS>
               MOVE WSAA-FROMPER       TO TRRN-FROMPER                  <PHLRMS>
                                                                        <PHLRMS>
               MOVE WSAA-TOPFX         TO TRRN-TOPFX                    <PHLRMS>
               MOVE WSAA-TOLOC         TO TRRN-TOLOC                    <PHLRMS>
               MOVE SPACES             TO TRRN-TOPER                    <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  TV035-RCSTAT-01         NOT = SPACES                     <PHLRMS>
               MOVE TV035-RCSTAT-01    TO TRRN-RCSTAT                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  TV035-RCSTAT-02         NOT = SPACES                     <PHLRMS>
               MOVE TV035-RCSTAT-02    TO TRRN-STATCODE                 <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE WSSP-USERID            TO TRRN-CRUSER .                 <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Get user's office                                            <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     TV023-TV023-REC.              <PHLRMS>
           MOVE TV023                  TO WSAA-TABLE.                   <PHLRMS>
           MOVE WSSP-USERID            TO WSAA-ITEM.                    <PHLRMS>
           PERFORM 4800-READ-TABLE.                                     <PHLRMS>
           MOVE ITEM-GENAREA           TO TV023-TV023-REC.              <PHLRMS>
           MOVE TV023-WOFFCODE         TO TRRN-OFFPRC.                  <PHLRMS>
                                                                        <PHLRMS>
           MOVE WSAA-TODAY             TO TRRN-TRANDATE.                <PHLRMS>
           MOVE WSKY-BATC-BATCTRCDE    TO TRRN-TRANCDE.                 <PHLRMS>
           MOVE WSAA-TRANNO            TO TRRN-TRANNO.                  <PHLRMS>
           MOVE SPACES                 TO TRRN-RSCDE.                   <PHLRMS>
                                                                        <PHLRMS>
           MOVE WRITR                  TO TRRN-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'TRRNIO'               USING TRRN-PARAMS.               <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRN-STATUZ             NOT = O-K                        <PHLRMS>
               MOVE TRRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               MOVE TRRN-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       4590-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       4600-READ-TV035 SECTION.                                         <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       4610-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     TV035-TV035-REC.              <PHLRMS>
           MOVE TV035                  TO WSAA-TABLE.                   <PHLRMS>
           MOVE WSKY-BATC-BATCTRCDE    TO WSAA-ITEM.                    <PHLRMS>
           PERFORM 4800-READ-TABLE.                                     <PHLRMS>
           MOVE ITEM-GENAREA           TO TV035-TV035-REC.              <PHLRMS>
      *                                                                 <PHLRMS>
       4690-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       4700-UPDATE-RCOL SECTION.                                        <PHLRMS>
      **************************                                        <PHLRMS>
      *                                                                 <PHLRMS>
       4710-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     RCOLPND-PARAMS.               <PHLRMS>
           MOVE WSSP-SERVUNIT          TO RCOLPND-RCTYPE.               <PHLRMS>
           MOVE WSSP-COMMON-AREA(102:8)                                 <PHLRMS>
                                       TO RCOLPND-RCSERNUM.             <PHLRMS>
           MOVE RCOLPNDREC             TO RCOLPND-FORMAT.               <PHLRMS>
           MOVE READR                  TO RCOLPND-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'RCOLPNDIO'            USING RCOLPND-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  RCOLPND-STATUZ          NOT = O-K                        <PHLRMS>
               MOVE RCOLPND-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               MOVE RCOLPND-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Update Contract Number                                       <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE WSAA-POLNO             TO RCOLPND-CHDRNUM.              <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Validflag = 1                                                <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE '1'                    TO RCOLPND-VALIDFLAG.            <PHLRMS>
                                                                        <PHLRMS>
           MOVE WRITD                  TO RCOLPND-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'RCOLPNDIO'            USING RCOLPND-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  RCOLPND-STATUZ          NOT = O-K                        <PHLRMS>
               MOVE RCOLPND-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE RCOLPND-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       4790-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       4800-READ-TABLE SECTION.                                         <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       4810-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                    ITEM-PARAMS.                   <PHLRMS>
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
               MOVE ITEM-PARAMS       TO SYSR-PARAMS                    <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       4890-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       4900-GET-NEW-TRANNO SECTION.                                     <PHLRMS>
      ****************************                                      <PHLRMS>
      *                                                                 <PHLRMS>
       4910-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE ZERO                   TO WSAA-TRANNO.                  <PHLRMS>
           INITIALIZE                     TRRNTRN-PARAMS.               <PHLRMS>
           MOVE WSSP-SERVUNIT          TO WSAA-RCPT-TYPE.               <PHLRMS>
           MOVE WSSP-COMMON-AREA(102:8)                                 <PHLRMS>
                                       TO WSAA-RCPT-NUM.                <PHLRMS>
           MOVE WSAA-RCPT-TYPE         TO TRRNTRN-RCTYPE.               <PHLRMS>
           MOVE WSAA-RCPT-NUM          TO TRRNTRN-RCSERNUM.             <PHLRMS>
           MOVE 99999                  TO TRRNTRN-TRANNO.               <PHLRMS>
           MOVE TRRNTRNREC             TO TRRNTRN-FORMAT.               <PHLRMS>
           MOVE BEGN                   TO TRRNTRN-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'TRRNTRNIO'            USING TRRNTRN-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF TRRNTRN-STATUZ           NOT = O-K                        <PHLRMS>
           AND                         NOT = ENDP                       <PHLRMS>
               MOVE TRRNTRN-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE TRRNTRN-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF TRRNTRN-STATUZ           NOT = O-K                        <PHLRMS>
           OR TRRNTRN-RCTYPE           NOT = WSAA-RCPT-TYPE             <PHLRMS>
           OR TRRNTRN-RCSERNUM         NOT = WSAA-RCPT-NUM              <PHLRMS>
               MOVE 1                  TO WSAA-TRANNO                   <PHLRMS>
               GO TO 4990-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           COMPUTE WSAA-TRANNO         = TRRNTRN-TRANNO + 1.            <PHLRMS>
      *                                                                 <PHLRMS>
       4990-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
                                                                        <V66P11>
       5500-CHECK-PRIOR-SFL-REC SECTION.                                <V65F04>
      **********************************                                <V65F04>
       5510-START.                                                      <V65F04>
           INITIALIZE                     WSAA-DUPL-WH-SACSCODE         <V65F04>
                                          WSAA-WH-SUBFILE-RRN.          <V65F04>
                                                                        <V65F04>
           MOVE SCRN-SUBFILE-RRN       TO WSAA-WH-SUBFILE-RRN.          <V65F04>
           SUBTRACT 1                FROM SCRN-SUBFILE-RRN.             <V65F04>
           MOVE SREAD                  TO SCRN-FUNCTION.                <V65F04>
                                                                        <V65F04>
           CALL 'S2610IO'           USING SCRN-SCREEN-PARAMS            <V65F04>
                                          S2610-DATA-AREA               <V65F04>
                                          S2610-SUBFILE-AREA.           <V65F04>
                                                                        <V65F04>
           IF  SCRN-STATUZ          NOT = O-K                           <V65F04>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <V65F04>
               PERFORM 600-FATAL-ERROR                                  <V65F04>
           END-IF.                                                      <V65F04>
                                                                        <V65F04>
           IF  S2610-SACSCODE           = 'WH'                          <V65F04>
               MOVE 'Y'                TO WSAA-DUPL-WH-SACSCODE         <V65F04>
           END-IF.                                                      <V65F04>
                                                                        <V65F04>
           MOVE WSAA-WH-SUBFILE-RRN    TO SCRN-SUBFILE-RRN.             <V65F04>
           MOVE SREAD                  TO SCRN-FUNCTION.                <V65F04>
                                                                        <V65F04>
           CALL 'S2610IO'           USING SCRN-SCREEN-PARAMS            <V65F04>
                                          S2610-DATA-AREA               <V65F04>
                                          S2610-SUBFILE-AREA.           <V65F04>
                                                                        <V65F04>
           IF  SCRN-STATUZ          NOT = O-K                           <V65F04>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <V65F04>
               PERFORM 600-FATAL-ERROR                                  <V65F04>
           END-IF.                                                      <V65F04>
                                                                        <V65F04>
       5590-EXIT.                                                       <V65F04>
           EXIT.                                                        <V65F04>
      /                                                                 <GP5152>
       5600-CHECK-DUPLICATES SECTION.                                   <GP5152>
      *******************************                                   <GP5152>
       5610-START.                                                      <GP5152>
                                                                        <GP5152>
           MOVE SCRN-SUBFILE-RRN       TO WSAA-DP-SUBFILE-RRN.          <GP5152>
           MOVE S2610-SACSCODE         TO WSAA-DP-SACSCODE              <GP5152>
           MOVE S2610-SACSTYPW         TO WSAA-DP-SACSTYPW              <GP5152>
           MOVE S2610-ENTITY           TO WSAA-DP-ENTITY                <GP5152>
                                                                        <PHE010>
           IF  S2610-SACSCODE           = 'LN'                          <PHE010>
               MOVE G317             TO S2610-SACSCODE-ERR              <PHE010>
               MOVE G317             TO S2610-FSACSCODE-ERR             <PHE010>
           END-IF.                                                      <PHE010>
                                                                        <GP5152>
           MOVE SPACES                 TO WSAA-IN-LOOP-STATUZ.          <GP5152>
           PERFORM 5700-CHECK-DUP-SUBFILE                               <GP5152>
                           UNTIL WSAA-IN-LOOP-STATUZ   = ENDP.          <GP5152>
                                                                        <GP5152>
           ADD    1                    TO WSAA-DP-SUBFILE-RRN           <GP5152>
           MOVE WSAA-DP-SUBFILE-RRN    TO SCRN-SUBFILE-RRN              <GP5152>
           MOVE SREAD                  TO SCRN-FUNCTION                 <GP5152>
                                                                        <GP5152>
           CALL 'S2610IO'           USING SCRN-SCREEN-PARAMS            <GP5152>
                                          S2610-DATA-AREA               <GP5152>
                                          S2610-SUBFILE-AREA            <GP5152>
                                                                        <GP5152>
           IF  SCRN-STATUZ              = MRNF                          <GP5152>
               MOVE ENDP               TO WSAA-OUT-LOOP-STATUZ          <GP5152>
               GO TO 5690-EXIT                                          <GP5152>
           END-IF.                                                      <GP5152>
                                                                        <GP5152>
           IF  SCRN-STATUZ          NOT = O-K                           <GP5152>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <GP5152>
                 PERFORM 600-FATAL-ERROR                                <GP5152>
           END-IF.                                                      <GP5152>
                                                                        <GP5152>
       5690-EXIT.                                                       <GP5152>
           EXIT.                                                        <GP5152>
      /                                                                 <GP5152>
       5700-CHECK-DUP-SUBFILE SECTION.                                  <GP5152>
      ********************************                                  <GP5152>
       5700-BEGIN.                                                      <GP5152>
                                                                        <GP5152>
           IF   S2610-SACSCODE          = SPACES AND                    <GP5152>
                S2610-SACSTYPW          = SPACES AND                    <GP5152>
                S2610-ENTITY            = SPACES                        <GP5152>
                MOVE ENDP              TO WSAA-IN-LOOP-STATUZ           <GP5152>
                GO TO 5700-EXIT.                                        <GP5152>
                                                                        <GP5152>
            MOVE SRDN                   TO SCRN-FUNCTION.               <GP5152>
                                                                        <GP5152>
            MOVE S2610-SACSCODE         TO WSAA-KEY-SACSCODE.           <GP5152>
            MOVE S2610-SACSTYPW         TO WSAA-KEY-SACSTYPW.           <GP5152>
            MOVE S2610-ENTITY           TO WSAA-KEY-ENTITY.             <GP5152>
                                                                        <GP5152>
            IF  SCRN-SUBFILE-RRN     NOT = WSAA-DP-SUBFILE-RRN          <GP5152>
                   IF  WSAA-KEY          = WSAA-DP-KEY                  <GP5152>
                       MOVE 'Y'         TO WSAA-FOUND-DUP               <GP5152>
                       MOVE H357        TO S2610-SACSCODE-ERR           <GP5152>
                                           S2610-SACSTYPW-ERR           <GP5152>
                                           S2610-ENTITY-ERR             <GP5152>
                       MOVE 'Y'         TO WSSP-EDTERROR                <GP5152>
                       MOVE SUPD        TO SCRN-FUNCTION                <GP5152>
                       CALL 'S2610IO' USING SCRN-SCREEN-PARAMS          <GP5152>
                                            S2610-DATA-AREA             <GP5152>
                                            S2610-SUBFILE-AREA          <GP5152>
                      IF SCRN-STATUZ   NOT = O-K  AND ENDP              <GP5152>
                         MOVE SCRN-STATUZ TO SYSR-STATUZ                <GP5152>
                         PERFORM 600-FATAL-ERROR                        <GP5152>
                      END-IF                                            <GP5152>
                    MOVE SRDN             TO SCRN-FUNCTION              <GP5152>
                END-IF                                                  <GP5152>
            END-IF.                                                     <GP5152>
                                                                        <GP5152>
            CALL 'S2610IO' USING SCRN-SCREEN-PARAMS                     <GP5152>
                                    S2610-DATA-AREA                     <GP5152>
                                    S2610-SUBFILE-AREA.                 <GP5152>
                                                                        <GP5152>
            IF  SCRN-STATUZ            =  ENDP                          <GP5152>
                MOVE SPACES            TO  SCRN-STATUZ                  <GP5152>
                MOVE ENDP              TO  WSAA-IN-LOOP-STATUZ          <GP5152>
                GO TO 5700-EXIT                                         <GP5152>
            END-IF                                                      <GP5152>
                                                                        <GP5152>
            IF  SCRN-STATUZ            = O-K                            <GP5152>
                NEXT SENTENCE                                           <GP5152>
            ELSE                                                        <GP5152>
                MOVE SCRN-STATUZ       TO SYSR-STATUZ                   <GP5152>
                PERFORM 600-FATAL-ERROR                                 <GP5152>
            END-IF.                                                     <GP5152>
                                                                        <GP5152>
       5700-EXIT.                                                       <GP5152>
            EXIT.                                                       <GP5152>
                                                                        <V65F04>
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
      /                                                                 <V76F06>
       9000-S2610-IO SECTION.
      ************************
       9000-SCREEN.

           CALL 'S2610IO' USING SCRN-SCREEN-PARAMS
                                S2610-DATA-AREA
                                S2610-SUBFILE-AREA.

           IF SCRN-STATUZ           NOT = O-K AND ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

       9000-EXIT.
            EXIT.
      /
       9100-SR289-IO SECTION.
      ************************
       9100-SCREEN.

           CALL 'SR289IO' USING SCRN-SCREEN-PARAMS
                                SR289-DATA-AREA
                                SR289-SUBFILE-AREA.

           IF SCRN-STATUZ           NOT = O-K AND ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

       9100-EXIT.
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
       B1010-READR-GENL SECTION.
      **************************
       B1010-BEGIN.
      *
           MOVE READR                  TO GENL-FUNCTION.
           MOVE GENLREC                TO GENL-FORMAT.
           CALL 'GENLIO'            USING GENL-PARAMS.

           IF  GENL-STATUZ          NOT = O-K AND MRNF
               MOVE GENL-STATUZ        TO SYSR-STATUZ
               MOVE GENL-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
       B1010-EXIT.
            EXIT.
      /
       B1020-READR-DESC SECTION.
      **************************
       B1020-BEGIN.
           MOVE READR                  TO DESC-FUNCTION.
           MOVE DESCREC                TO DESC-FORMAT.
           CALL 'DESCIO'            USING DESC-PARAMS.

           IF DESC-STATUZ              NOT = O-K AND MRNF
              MOVE DESC-STATUZ         TO SYSR-STATUZ
              MOVE DESC-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
       B1020-EXIT.
            EXIT.
      /
       B1030-READH-BBAL SECTION.
      **************************
       B1030-BEGIN.
           MOVE BBALREC                TO BBAL-FORMAT.
           MOVE READH                  TO BBAL-FUNCTION.
           CALL 'BBALIO'            USING BBAL-PARAMS.

           IF BBAL-STATUZ           NOT = O-K AND MRNF
              MOVE BBAL-STATUZ         TO SYSR-STATUZ
              MOVE BBAL-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

       B1030-EXIT.
            EXIT.
                                                                        <V62P15>
       B2000-AT-REQUEST SECTION.                                        <V62P15>
      ***************************                                       <V62P15>
       B2010-AT.                                                        <V62P15>
                                                                        <PS2509>
      *--- READ TR272 TO DECIDE WHETHER TO CALL OR NOT                  <PS2509>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <PS2509>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <PS2509>
           MOVE TR272                  TO ITEM-ITEMTABL.                <PS2509>
           MOVE WSAA-CL-STAR           TO ITEM-ITEMITEM.                <PS2509>
                                                                        <PS2509>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <PS2509>
           MOVE READR                  TO ITEM-FUNCTION.                <PS2509>
           CALL  'ITEMIO'           USING ITEM-PARAMS.                  <PS2509>
           IF ITEM-STATUZ           NOT = O-K                           <PS2509>
              GO                       TO B2090-EXIT                    <PS2509>
           END-IF.                                                      <PS2509>
                                                                        <V62P15>
           INITIALIZE                     ATRT-ATREQ-REC.               <V62P15>
           MOVE 'PRTCLMF'              TO ATRT-MODULE.                  <V62P15>
           MOVE WSSP-BATCHKEY          TO ATRT-BATCH-KEY.               <V62P15>
           MOVE WSAA-PROG              TO ATRT-REQ-PROG.                <V62P15>
           MOVE WSSP-TRANID            TO VRCM-TRANID.                  <V62P15>
           MOVE VRCM-USER              TO ATRT-REQ-USER.                <V62P15>
           MOVE VRCM-TERMID            TO ATRT-REQ-TERM.                <V62P15>
           MOVE WSAA-TODAY             TO ATRT-REQ-DATE.                <V62P15>
           MOVE VRCM-TIME              TO ATRT-REQ-TIME.                <V62P15>
           MOVE WSSP-LANGUAGE          TO ATRT-LANGUAGE.                <V62P15>
           MOVE WSSP-DOCTKEY           TO ATRT-PRIMARY-KEY.             <V62P15>
                                                                        <V62P15>
           INITIALIZE                     PRTC-PRTCLMF-REC.             <V62P15>
           MOVE WSAA-RECEIPT-PFX       TO PRTC-RDOCPFX.                 <V62P15>
           MOVE WSAA-RECEIPT-COY       TO PRTC-RDOCCOY.                 <V62P15>
           MOVE WSAA-RECEIPT           TO PRTC-RDOCNUM.                 <V62P15>
           MOVE WSSP-LANGUAGE          TO PRTC-LANGUAGE.                <V62P15>
           MOVE WSSP-COMPANY           TO PRTC-COMPANY.                 <V62P15>
           MOVE WSSP-FSUCO             TO PRTC-FSU-COMPANY.             <V62P15>
           MOVE PRTC-PRTCLMF-REC       TO ATRT-TRANS-AREA.              <V62P15>
                                                                        <V62P15>
           CALL 'ATREQ'             USING ATRT-ATREQ-REC.               <V62P15>
                                                                        <V62P15>
           IF ATRT-STATUZ           NOT = O-K                           <V62P15>
              MOVE ATRT-STATUZ         TO SYSR-STATUZ                   <V62P15>
              MOVE ATRT-ATREQ-REC      TO SYSR-PARAMS                   <V62P15>
              PERFORM 600-FATAL-ERROR                                   <V62P15>
           END-IF.                                                      <V62P15>
                                                                        <V62P15>
       B2090-EXIT.                                                      <V62P15>
            EXIT.                                                       <V62P15>
       B3000-UP-ADOC SECTION.                                           <FA3384>
      ***********************                                   <FA3384><FA3384>
       B3010-READ-RDOC.                                                 <FA3384>
           INITIALIZE                  RDOC-PARAMS.                     <FA3384>
           MOVE PRFX-CASH              TO RDOC-RDOCPFX.                 <FA3384>
           MOVE WSSP-COMPANY           TO RDOC-RDOCCOY.                 <FA3384>
           MOVE WSAA-OLD-RECEIPT       TO RDOC-RDOCNUM.                 <FA3384>
           MOVE S2610-TRANSEQ          TO RDOC-TRANSEQ.                 <FA3384>
           MOVE READH                  TO RDOC-FUNCTION.                <FA3384>
           MOVE RDOCREC                TO RDOC-FORMAT.                  <FA3384>
           CALL 'RDOCIO'         USING RDOC-PARAMS.                     <FA3384>
                                                                        <FA3384>
           IF RDOC-STATUZ              = MRNF                           <FA3384>
              MOVE RDOC-STATUZ         TO SYSR-STATUZ                   <FA3384>
              MOVE RDOC-PARAMS         TO SYSR-PARAMS                   <FA3384>
              PERFORM 600-FATAL-ERROR                                   <FA3384>
           END-IF.                                                      <FA3384>
                                                                        <FA3384>
           MOVE WSAA-NEW-RECEIPT       TO RDOC-RLDGACCT.                <FA3384>
           MOVE REWRT                  TO RDOC-FUNCTION.                <FA3384>
           CALL 'RDOCIO'         USING RDOC-PARAMS.                     <FA3384>
                                                                        <FA3384>
           IF RDOC-STATUZ          NOT = O-K                            <FA3384>
              MOVE RDOC-STATUZ         TO SYSR-STATUZ                   <FA3384>
              MOVE RDOC-PARAMS         TO SYSR-PARAMS                   <FA3384>
              PERFORM 600-FATAL-ERROR                                   <FA3384>
           END-IF.                                                      <FA3384>
      *                                                         <FA3384><FA3384>
       A3000-EXIT.                                                      <FA3384>
           EXIT.                                                        <FA3384>
      *                                                         <FA3384><FA3384>
       A2000-RETREIVE-FROM-SR22P SECTION.                               <V71F07>
      ***********************************                               <V71F07>
       A2010-FROM-SR22P.                                                <V71F07>
                                                                        <V71F07>
           MOVE 'N'                    TO WSAA-SR22P-NEW-REC.           <V71F07>
           MOVE 'N'                    TO WSAA-NO-MORE-BLANC-LINE.      <V71F07>
           MOVE  0                     TO WSAA-SFL-CNT.                 <V71F07>
                                                                        <V71F07>
   ****Call SFLSAVE to retreive the RRN of records picked up in SR22P   <V71F07>
   ****                                                                 <V71F07>
           MOVE RETRV                  TO SFLS-FUNCTION.                <V71F07>
           CALL 'SFLSAVE'           USING SFLS-SFLSAVE-REC.             <V71F07>
                                                                        <V71F07>
           IF SFLS-STATUZ           NOT = O-K AND MRNF                  <V71F07>
              MOVE SFLS-STATUZ         TO SYSR-STATUZ                   <V71F07>
              PERFORM 600-FATAL-ERROR                                   <V71F07>
           END-IF.                                                      <V71F07>
           IF SFLS-STATUZ               = MRNF                          <V71F07>
              GO TO A2090-EXIT                                          <V71F07>
           END-IF.                                                      <V71F07>
                                                                        <V71F07>
      ****Load selected records in S2610                                <V71F07>
      ****                                                              <V71F07>
           MOVE SSTRT                         TO SCRN-FUNCTION.         <V71F07>
           PERFORM 9000-S2610-IO.                                       <V71F07>
           MOVE 1                      TO WSAA-IX.                      <V71F07>
           PERFORM  A2200-LOAD-SUBFILE-SFLSAVE                          <V71F07>
                       UNTIL SFLS-UNIQUE-KEY(WSAA-IX) = ZEROS.          <V71F07>
                                                                        <V71F07>
      ****Clear SFLSAVE                                                 <V71F07>
      ****                                                              <V71F07>
           MOVE RLSE                   TO SFLS-FUNCTION.                <V71F07>
           CALL 'SFLSAVE'           USING SFLS-SFLSAVE-REC.             <V71F07>
                                                                        <V71F07>
           IF SFLS-STATUZ           NOT = O-K                           <V71F07>
              MOVE SFLS-STATUZ         TO SYSR-STATUZ                   <V71F07>
              PERFORM 600-FATAL-ERROR                                   <V71F07>
           END-IF.                                                      <V71F07>
                                                                        <V71F07>
      **** Load blank subfile line if subfile page incomplete           <V71F07>
      ****                                                              <V71F07>
           IF WSAA-SFL-CNT              > 0                             <V71F07>
              PERFORM 1312-LOAD-BLANK-SUBFILE UNTIL                     <V71F07>
                             WSAA-SFL-CNT > S2610-SUBFILE-PAGE          <V71F07>
              MOVE 'Y'             TO SCRN-SUBFILE-MORE                 <V71F07>
           END-IF.                                                      <V71F07>
                                                                        <V71F07>
      *                                                                 <V71F07>
       A2090-EXIT.                                                      <V71F07>
           EXIT.                                                        <V71F07>
                                                                        <V71F07>
       A2100-GET-NEXT-BLANC-S2610SFL SECTION.                           <V71F07>
      ***************************************                           <V71F07>
       A2110-S2610.                                                     <V71F07>
                                                                        <V71F07>
           PERFORM UNTIL (S2610-SACSCODE        = SPACES AND            <V71F07>
                         S2610-SACSTYPW        = SPACES AND             <V71F07>
                         S2610-ORIGAMT         = ZEROES AND             <V71F07>
                         S2610-ACCTAMT         = ZEROES) OR             <V71F07>
                         NO-MORE-BLANC-LINE                             <V71F07>
              MOVE SRDN                    TO SCRN-FUNCTION             <V71F07>
              PERFORM 9000-S2610-IO                                     <V71F07>
              IF SCRN-STATUZ                = ENDP                      <V71F07>
                MOVE 'Y'                   TO WSAA-NO-MORE-BLANC-LINE   <V71F07>
                MOVE  1                    TO WSAA-SFL-CNT              <V71F07>
              END-IF                                                    <V71F07>
           END-PERFORM.                                                 <V71F07>
      *                                                                 <V71F07>
       A2190-EXIT.                                                      <V71F07>
           EXIT.                                                        <V71F07>
                                                                        <V71F07>
       A2200-LOAD-SUBFILE-SFLSAVE SECTION.                              <V71F07>
      ***********************************                               <V71F07>
       A2210-SUBFILE.                                                   <V71F07>
      *                                                                 <V71F07>
           PERFORM A2100-GET-NEXT-BLANC-S2610SFL.                       <V71F07>
      *                                                                 <V71F07>
           MOVE SCRN-SUBFILE-RRN           TO WSAA-SFL-RRN.             <V71F07>
           MOVE SCRN-SUBFILE-END           TO WSAA-SFL-END.             <V71F07>
      *                                                                 <V71F07>
           MOVE SFLS-SUBFILE-RRN (WSAA-IX) TO SCRN-SUBFILE-RRN.         <V71F07>
           MOVE SFLS-SUBFILE-END           TO SCRN-SUBFILE-END.         <V71F07>
           MOVE SREAD                      TO SCRN-FUNCTION.            <V71F07>
           PERFORM A2400-SR22PIO.                                       <V71F07>
           IF SCRN-STATUZ               NOT = O-K                       <V71F07>
              MOVE SCRN-STATUZ             TO SYSR-STATUZ               <V71F07>
              PERFORM 600-FATAL-ERROR                                   <V71F07>
           END-IF.                                                      <V71F07>
      *                                                                 <V71F07>
      *---LOAD FROM SR22P                                               <V71F07>
           PERFORM A2300-MOVE-FROM-SR22P.                               <V71F07>
      *                                                                 <V71F07>
           MOVE WSAA-SFL-RRN          TO SCRN-SUBFILE-RRN.              <V71F07>
           MOVE WSAA-SFL-END          TO SCRN-SUBFILE-END.              <V71F07>
      *                                                                 <V71F07>
           IF NO-MORE-BLANC-LINE                                        <V71F07>
              MOVE SADD               TO SCRN-FUNCTION                  <V71F07>
              IF WSAA-SFL-CNT          > S2610-SUBFILE-PAGE             <V71F07>
                 MOVE 'Y'             TO SCRN-SUBFILE-MORE              <V71F07>
                 MOVE 1               TO WSAA-SFL-CNT                   <V71F07>
              END-IF                                                    <V71F07>
              ADD 1                   TO WSAA-SFL-CNT                   <V71F07>
           ELSE                                                         <V71F07>
              MOVE SUPD                  TO SCRN-FUNCTION               <V71F07>
           END-IF.                                                      <V71F07>
                                                                        <V71F07>
           PERFORM 9000-S2610-IO.                                       <V71F07>
                                                                        <V71F07>
           IF SCRN-STATUZ          NOT = O-K                            <V71F07>
              MOVE SCRN-STATUZ        TO SYSR-STATUZ                    <V71F07>
              PERFORM 600-FATAL-ERROR                                   <V71F07>
           END-IF.                                                      <V71F07>
                                                                        <V71F07>
           MOVE 'Y'                   TO WSAA-SR22P-NEW-REC.            <V71F07>
                                                                        <V71F07>
                                                                        <V71F07>
           ADD 1                      TO WSAA-IX.                       <V71F07>
      *                                                                 <V71F07>
       A2290-EXIT.                                                      <V71F07>
            EXIT.                                                       <V71F07>
      /                                                                 <V71F07>
       A2300-MOVE-FROM-SR22P SECTION.                                   <V71F07>
      ******************************                                    <V71F07>
       A2310-SR22P.                                                     <V71F07>
      *                                                                 <V71F07>
           MOVE SR22P-FSACSCODE        TO S2610-SACSCODE.               <V71F07>
           MOVE SR22P-FSACSTYP         TO S2610-SACSTYPW.               <V71F07>
           MOVE SR22P-POLNUM           TO S2610-ENTITY.                 <V71F07>
           IF SR22P-SLT = 'G'                                           <V71F07>
              MOVE SR22P-ORGAMT        TO S2610-ORIGAMT                 <V71F07>
           ELSE                                                         <V71F07>
              MOVE SR22P-NET-AMT       TO S2610-ORIGAMT                 <V71F07>
           END-IF.                                                      <V71F07>
           MOVE ZEROES                 TO S2610-SELECT-NUMBER.          <V71F07>
           MOVE SPACES                 TO S2610-TRANDESC.               <V71F07>
           MOVE SPACES                 TO S2610-DISACC.                 <V71F07>
           MOVE SR22P-ORIG-CCY         TO S2610-BCURR.                  <V71F07>
           MOVE ZEROES                 TO S2610-SLRATE.                 <V71F07>
           MOVE ZEROES                 TO S2610-ACCTAMT.                <V71F07>
           MOVE WSSP-ACCTCCY           TO S2610-GENLCUR.                <V71F07>
           MOVE ZEROS                  TO S2610-AMNT                    <V71F07>
                                          S2610-CNT.                    <V71F07>
           MOVE SPACES                 TO S2610-INDIC.                  <V71F07>
                                                                        <V71F07>
           IF NO-MORE-BLANC-LINE                                        <V71F07>
              MOVE WSAA-TRANSEQ           TO S2610-TRANSEQ              <V71F07>
              ADD 1                       TO WSAA-TRANSEQ               <V71F07>
           END-IF.                                                      <V71F07>
      *                                                                 <V71F07>
       A2390-EXIT.                                                      <V71F07>
            EXIT.                                                       <V71F07>
      *                                                                 <V71F07>
       A2400-SR22PIO SECTION.                                           <V71F07>
      **********************                                            <V71F07>
       A2410-SR22PIO.                                                   <V71F07>
      *                                                                 <V71F07>
           CALL 'SR22PIO'           USING SCRN-SCREEN-PARAMS            <V71F07>
                                          SR22P-DATA-AREA               <V71F07>
                                          SR22P-SUBFILE-AREA.           <V71F07>
           IF SCRN-STATUZ           NOT = O-K AND ENDP AND MRNF         <V71F07>
              MOVE SCRN-STATUZ         TO SYSR-STATUZ                   <V71F07>
              PERFORM 600-FATAL-ERROR                                   <V71F07>
           END-IF.                                                      <V71F07>
      *                                                                 <V71F07>
       A2490-EXIT.                                                      <V71F07>
            EXIT.                                                       <V71F07>
      /                                                                 <V73G34>
       A100-READ-TR9QK SECTION.                                         <V73G34>
      *************************                                         <V73G34>
       A100-BEGIN.                                                      <V73G34>
                                                                        <V73G34>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <V73G34>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <V73G34>
           MOVE TR9QK                  TO ITEM-ITEMTABL.                <V73G34>
           MOVE WSSP-COMPANY           TO ITEM-ITEMITEM.                <V73G34>
           MOVE SPACES                 TO ITEM-ITEMSEQ.                 <V73G34>
           MOVE READR                  TO ITEM-FUNCTION.                <V73G34>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <V73G34>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <V73G34>
           IF ITEM-STATUZ              NOT = O-K  AND MRNF              <V73G34>
              MOVE ITEM-PARAMS         TO SYSR-PARAMS                   <V73G34>
              MOVE ITEM-STATUZ         TO SYSR-STATUZ                   <V73G34>
              PERFORM 600-FATAL-ERROR                                   <V73G34>
           END-IF.                                                      <V73G34>
           IF ITEM-STATUZ   =  MRNF                                     <V73G34>
              MOVE SPACES              TO TR9QK-VALIDCHQ                <V73G34>
           ELSE                                                         <V73G34>
              MOVE ITEM-GENAREA        TO TR9QK-TR9QK-REC               <V73G34>
           END-IF.                                                      <V73G34>
                                                                        <V73G34>
       A100-EXIT.                                                       <V73G34>
           EXIT.                                                        <V73G34>
                                                                        <V74G02>
       A200-CALL-SUBACCBAL  SECTION.                                    <V74G02>
       A200-BEGIN.                                                      <V74G02>
      *    The SUBACCBAL routine will check the subsidary ledger account<V74G02>
      *    balance on ACBL and any pending payment requests to see if   <V74G02>
      *    the dissection will cause an unacceptable negitive balance.  <V74G02>
      *                                                                 <V74G02>
           MOVE ZEROES                 TO WSAA-NEW-BAL.                 <V74G02>
           INITIALIZE                     SABL-SUBACCBAL-REC.           <V74G02>
           MOVE 'TOTAL'                TO SABL-FUNCTION.                <V74G02>
           MOVE WSSP-BATCHKEY          TO SABL-BATCHKEY.                <V74G02>
           MOVE WSSP-FSUCO             TO SABL-FSUCO.                   <V74G02>
           MOVE WSSP-FLAG              TO SABL-ACTION.                  <V74G02>
           MOVE WSAA-TRANPFX           TO SABL-RLDGPFX.                 <V74G02>
           MOVE WSSP-COMPANY           TO SABL-RLDGCOY.                 <V74G02>
           MOVE S2610-ENTITY           TO SABL-RLDGACCT.                <V74G02>
           MOVE WSSP-RDOCNUM           TO SABL-RDOCNUM.                 <V74G02>
           MOVE S2610-SACSCODE         TO SABL-SACSCODE.                <V74G02>
           MOVE S2610-SACSTYPW         TO SABL-SACSTYP.                 <V74G02>
           MOVE WSSP-ORIGCCY           TO SABL-ORIGCURR.                <V74G02>
           MOVE S2610-ORIGAMT          TO SABL-ORIGAMT.                 <V74G02>
           MOVE WSSP-TRANDATE          TO SABL-EFFDATE.                 <V74G02>
                                                                        <V74G02>
           CALL 'SUBACCBAL' USING SABL-SUBACCBAL-REC.                   <V74G02>
                                                                        <V74G02>
           IF  SABL-STATUZ             = 'NOCK'                         <V74G02>
      *        No balance check is required                             <V74G02>
               GO TO A200-EXIT                                          <V74G02>
           ELSE                                                         <V74G02>
           IF  SABL-STATUZ             = BOMB                           <V74G02>
               MOVE 'SUBACCBAL'        TO SYSR-SUBRNAME                 <V74G02>
               MOVE SABL-STATUZ        TO SYSR-STATUZ                   <V74G02>
           ELSE                                                         <V74G02>
           IF  SABL-STATUZ         NOT = O-K                            <V74G02>
               MOVE SABL-STATUZ        TO S2610-ENTITY-ERR              <V74G02>
               GO TO A200-EXIT.                                         <V74G02>
      *                                                                 <V74G02>
           IF  SABL-TOT-BAL   >  ZERO                                   <V74G02>
           COMPUTE WSAA-NEW-BAL = SABL-TOT-BAL + S2610-ORIGAMT          <V74G02>
           ELSE                                                         <V74G02>
           COMPUTE WSAA-NEW-BAL = SABL-TOT-BAL - S2610-ORIGAMT          <V74G02>
           END-IF.                                                      <V74G02>
      *                                                                 <V74G02>
           IF WSAA-NEW-BAL > ZERO                                       <V74G02>
               MOVE E778               TO S2610-ENTITY-ERR.             <V74G02>
       A200-EXIT.                                                       <V74G02>
            EXIT.                                                       <V74G02>
      /                                                                 <V76F02>
       A300-READ-T3605 SECTION.                                         <V76F02>
      *************************                                         <V76F02>
       A300-BEGIN.                                                      <V76F02>
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.                 <V76F02>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <V76F02>
           MOVE T3605                  TO ITEM-ITEMTABL.                <V76F02>
           MOVE WSSP-COMPANY           TO ITEM-ITEMITEM.                <V76F02>
           MOVE READR                  TO ITEM-FUNCTION.                <V76F02>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <V76F02>
           IF ITEM-STATUZ              NOT = O-K AND MRNF               <V76F02>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <V76F02>
               PERFORM 600-FATAL-ERROR                                  <V76F02>
           END-IF                                                       <V76F02>
                                                                        <V76F02>
           IF ITEM-STATUZ               = O-K                           <V76F02>
              MOVE ITEM-GENAREA        TO T3605-T3605-REC               <V76F02>
           ELSE                                                         <V76F02>
              MOVE SPACES              TO T3605-T3605-REC               <V76F02>
           END-IF.                                                      <V76F02>
       A300-EXIT.                                                       <V76F02>
           EXIT.                                                        <V76F02>
      /                                                                 <V76F02>
       A400-DISPLAY-USERNAME SECTION.                                   <V76F02>
      *******************************                                   <V76F02>
       A410-DISPLAY.                                                    <V76F02>
           IF SBM-CREATION                                              <V76F02>
           OR SBM-MODIFY                                                <V76F12>
              MOVE VRCM-USER           TO WSKY-SANC-SANCUSR             <V76F02>
              MOVE WSSP-USERID         TO S2610-USERID                  <V76F02>
              GO TO A420-DISPLAY-NAME                                   <V76F02>
           END-IF.                                                      <V76F02>
                                                                        <V76F02>
      *    INITIALIZE                     ADOC-DATA-AREA.       <V76F12><V76F02>
      *    MOVE PRFX-CASH              TO ADOC-RTRN-RDOCPFX.    <V76F12><V76F02>
      *    MOVE WSSP-COMPANY           TO ADOC-RTRN-RDOCCOY.    <V76F12><V76F02>
      *    MOVE WSAA-RECEIPT           TO ADOC-RTRN-RDOCNUM.    <V76F12><V76F02>
      *    MOVE ZEROES                 TO ADOC-RTRN-TRANNO.     <V76F12><V76F02>
      *    MOVE ZEROES                 TO ADOC-RTRN-JRNSEQ.     <V76F12><V76F02>
      *    MOVE RTRNREC                TO ADOC-FORMAT.          <V76F12><V76F02>
      *    MOVE BEGN                   TO ADOC-FUNCTION.        <V76F12><V76F02>
      *                                                         <V76F12><V76F02>
      *    CALL 'ADOCIO'            USING ADOC-PARAMS.          <V76F12><V76F02>
      *    IF ADOC-STATUZ           NOT = O-K AND ENDP          <V76F12><V76F02>
      *       MOVE ADOC-PARAMS         TO SYSR-PARAMS           <V76F12><V76F02>
      *       MOVE ADOC-STATUZ         TO SYSR-STATUZ           <V76F12><V76F02>
      *       PERFORM 600-FATAL-ERROR                           <V76F12><V76F02>
      *    END-IF.                                              <V76F12><V76F02>
      *                                                         <V76F12><V76F02>
      *    MOVE WSAA-BATCKEY           TO BATC-DATA-KEY.        <V76F12><V76F02>
      *    MOVE ADOC-RTRN-BATCACTYR    TO BATC-BATCACTYR.       <V76F12><V76F02>
      *    MOVE ADOC-RTRN-BATCACTMN    TO BATC-BATCACTMN.       <V76F12><V76F02>
      *    MOVE ADOC-RTRN-BATCTRCDE    TO BATC-BATCTRCDE.       <V76F12><V76F02>
      *    MOVE ADOC-RTRN-BATCBATCH    TO BATC-BATCBATCH.       <V76F12><V76F02>
      *    MOVE READR                  TO BATC-FUNCTION.        <V76F12><V76F02>
      *    MOVE BATCREC                TO BATC-FORMAT.          <V76F12><V76F02>
      *                                                         <V76F12><V76F02>
      *    CALL 'BATCIO' USING BATC-PARAMS.                     <V76F12><V76F02>
      *    IF BATC-STATUZ              NOT = O-K                <V76F12><V76F02>
      *       MOVE BATC-PARAMS         TO SYSR-PARAMS           <V76F12><V76F02>
      *       PERFORM 600-FATAL-ERROR                           <V76F12><V76F02>
      *    END-IF.                                              <V76F12><V76F02>
      *                                                         <V76F12><V76F02>
      *    MOVE BATC-USERX             TO WSKY-SANC-SANCUSR.    <V76F12><V76F02>
           INITIALIZE                     RBNKSEQ-DATA-AREA.            <V76F12>
           MOVE PRFX-CASH              TO RBNKSEQ-RDOCPFX.              <V76F12>
           MOVE WSSP-COMPANY           TO RBNKSEQ-RDOCCOY.              <V76F12>
           MOVE WSAA-RECEIPT           TO RBNKSEQ-RDOCNUM.              <V76F12>
           MOVE ZEROES                 TO RBNKSEQ-SEQNBR.               <V76F12>
           MOVE BEGN                   TO RBNKSEQ-FUNCTION.             <V76F12>
                                                                        <V76F12>
           PERFORM C300-RBNKSEQIO.                                      <V76F12>
           IF  RBNKSEQ-STATUZ           = O-K                           <V76F12>
           AND RBNKSEQ-RDOCPFX          = PRFX-CASH                     <V76F12>
           AND RBNKSEQ-RDOCCOY          = WSSP-COMPANY                  <V76F12>
           AND RBNKSEQ-RDOCNUM          = WSAA-RECEIPT                  <V76F12>
               MOVE RBNKSEQ-USER       TO WSKY-SANC-SANCUSR             <V76F12>
           ELSE                                                         <V76F12>
               MOVE SPACES             TO S2610-USERID                  <V76F12>
                                          S2610-USRNAME                 <V76F12>
               GO TO A490-EXIT                                          <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F02>
       A420-DISPLAY-NAME.                                               <V76F02>
           MOVE WSSP-COMPANY           TO WSKY-SANC-SANCCOY.            <V76F02>
           MOVE WSKY-SANC-KEY          TO SANC-DATA-KEY.                <V76F02>
           MOVE READR                  TO SANC-FUNCTION.                <V76F02>
           MOVE SANCREC                TO SANC-FORMAT.                  <V76F02>
                                                                        <V76F02>
           CALL 'SANCEM'            USING SANC-PARAMS.                  <V76F02>
           IF SANC-STATUZ           NOT = O-K                           <V76F02>
              MOVE SANC-PARAMS         TO SYSR-PARAMS                   <V76F02>
              PERFORM 600-FATAL-ERROR                                   <V76F02>
           END-IF.                                                      <V76F02>
                                                                        <V76F02>
           MOVE SANC-USERNAME          TO S2610-USRNAME.                <V76F02>
           IF NOT SBM-CREATION                                          <V76F02>
              MOVE SANC-USER38NM       TO S2610-USERID                  <V76F02>
           END-IF.                                                      <V76F02>
       A490-EXIT.                                                       <V76F02>
           EXIT.                                                        <V76F02>
      /                                                                 <V76F12>
       A500-READ-TR29S SECTION.                                         <V76F12>
      *************************                                         <V76F12>
       A510-BEGIN.                                                      <V76F12>
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
                                                                        <V76F12>
           IF ITEM-STATUZ               = O-K                           <V76F12>
              MOVE ITEM-GENAREA        TO TR29S-TR29S-REC               <V76F12>
           ELSE                                                         <V76F12>
              MOVE SPACES              TO TR29S-TR29S-REC               <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE TR29S-APPFLAG          TO WSAA-RCTSTATFLG.              <V76F12>
       A590-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       A600-UNLOCK-DOCUMENT SECTION.                                    <V76F12>
      ******************************                                    <V76F12>
       A610-UNLOCK.                                                     <V76F12>
      *--Unlock document which is previously lock in Receipt menu.      <V76F12>
           IF SBM-CANCELLATION                                          <V76F12>
           OR SBM-PARTIAL-CANCEL                                        <V76F12>
           OR SBM-MODIFY                                                <V76F12>
              NEXT SENTENCE                                             <V76F12>
           ELSE                                                         <V76F12>
              GO TO A690-EXIT                                           <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE SPACES                 TO SFTL-SFTLOCK-REC.             <V76F12>
           MOVE O-K                    TO SFTL-STATUZ.                  <V76F12>
                                                                        <V76F12>
           MOVE WSSP-COMPANY           TO SFTL-COMPANY.                 <V76F12>
           MOVE PRFX-CASH              TO SFTL-ENTTYP.                  <V76F12>
                                                                        <V76F12>
      *--Get transaction code from RDOC/RCPT to get the acutal value    <V76F12>
      *--because during cancellation, the one we are locking is         <V76F12>
      *--the cancel document not document reversal.                     <V76F12>
           IF SBM-PARTIAL-CANCEL                                        <V76F12>
           OR SBM-CANCELLATION                                          <V76F12>
              MOVE WSAA-OLD-RECEIPT    TO SFTL-ENTITY                   <V76F12>
           ELSE                                                         <V76F12>
              MOVE WSAA-RECEIPT        TO SFTL-ENTITY                   <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE SPACES                 TO RDOC-DATA-KEY.                <V76F12>
           MOVE PRFX-CASH              TO RDOC-RDOCPFX.                 <V76F12>
           MOVE WSSP-COMPANY           TO RDOC-RDOCCOY.                 <V76F12>
           MOVE SFTL-ENTITY            TO RDOC-RDOCNUM.                 <V76F12>
           MOVE BEGN                   TO RDOC-FUNCTION.                <V76F12>
           MOVE RDOCREC                TO RDOC-FORMAT.                  <V76F12>
                                                                        <V76F12>
           CALL 'RDOCIO'            USING RDOC-PARAMS.                  <V76F12>
                                                                        <V76F12>
           IF RDOC-STATUZ               = O-K                           <V76F12>
           AND RDOC-RDOCPFX             = PRFX-CASH                     <V76F12>
           AND RDOC-RDOCCOY             = WSSP-COMPANY                  <V76F12>
           AND RDOC-RDOCNUM             = SFTL-ENTITY(1:9)              <V76F12>
              MOVE RDOC-BATCTRCDE     TO SFTL-TRANSACTION               <V76F12>
           ELSE                                                         <V76F12>
              MOVE PRFX-CASH           TO RCPT-RDOCPFX                  <V76F12>
              MOVE WSSP-COMPANY        TO RCPT-RDOCCOY                  <V76F12>
              MOVE SFTL-ENTITY         TO RCPT-RDOCNUM                  <V76F12>
              MOVE ZEROES              TO RCPT-SEQNBR                   <V76F12>
              MOVE RCPTREC             TO RCPT-FORMAT                   <V76F12>
              MOVE READR               TO RCPT-FUNCTION                 <V76F12>
              PERFORM C100-RCPTIO                                       <V76F12>
                                                                        <V76F12>
              IF RCPT-STATUZ      = O-K                                 <V76F12>
                 MOVE RCPT-TRANCDE     TO SFTL-TRANSACTION              <V76F12>
              END-IF                                                    <V76F12>
           END-IF.                                                      <V76F12>
                                                                        <V76F12>
           MOVE 'UNLK'                 TO SFTL-FUNCTION.                <V76F12>
           CALL 'SFTLOCK'              USING SFTL-SFTLOCK-REC.          <V76F12>
                                                                        <V76F12>
           IF SFTL-STATUZ              NOT = O-K                        <V76F12>
               MOVE SFTL-STATUZ        TO SYSR-STATUZ                   <V76F12>
               PERFORM 600-FATAL-ERROR                                  <V76F12>
           END-IF.                                                      <V76F12>
       A690-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       C100-RCPTIO SECTION.                                             <V76F12>
      *********************                                             <V76F12>
       C110-IO.                                                         <V76F12>
           MOVE RCPTREC                TO RCPT-FORMAT.                  <V76F12>
           CALL 'RCPTIO'            USING RCPT-PARAMS.                  <V76F12>
           IF  RCPT-STATUZ          NOT = O-K                           <V76F12>
               MOVE RCPT-PARAMS        TO SYSR-PARAMS                   <V76F12>
               MOVE RCPT-STATUZ        TO SYSR-STATUZ                   <V76F12>
               PERFORM 600-FATAL-ERROR                                  <V76F12>
           END-IF.                                                      <V76F12>
       C119-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       C200-RCPTIO SECTION.                                             <V76F12>
      *********************                                             <V76F12>
       C210-IO.                                                         <V76F12>
           MOVE RCPTREC                TO RCPT-FORMAT.                  <V76F12>
           CALL 'RCPTIO'            USING RCPT-PARAMS.                  <V76F12>
           IF  RCPT-STATUZ          NOT = O-K AND ENDP AND MRNF         <V76F12>
               MOVE RCPT-PARAMS        TO SYSR-PARAMS                   <V76F12>
               MOVE RCPT-STATUZ        TO SYSR-STATUZ                   <V76F12>
               PERFORM 600-FATAL-ERROR                                  <V76F12>
           END-IF.                                                      <V76F12>
       C219-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <V76F12>
       C300-RBNKSEQIO SECTION.                                          <V76F12>
      ************************                                          <V76F12>
       C310-IO.                                                         <V76F12>
           MOVE RBNKSEQREC             TO RBNKSEQ-FORMAT.               <V76F12>
           CALL 'RBNKSEQIO'         USING RBNKSEQ-PARAMS.               <V76F12>
           IF  RBNKSEQ-STATUZ       NOT = O-K AND ENDP AND MRNF         <V76F12>
               MOVE RBNKSEQ-PARAMS     TO SYSR-PARAMS                   <V76F12>
               MOVE RBNKSEQ-STATUZ     TO SYSR-STATUZ                   <V76F12>
               PERFORM 600-FATAL-ERROR                                  <V76F12>
           END-IF.                                                      <V76F12>
       C319-EXIT.                                                       <V76F12>
           EXIT.                                                        <V76F12>
      /                                                                 <RC002>
      * Update ZPPR in case Modify                                      <RC002>
      ***************************************************************** <RC002>
       A700-UPDATE-ZPPR SECTION.                                        <RC002>
      ************************                                  <RC002> <V76F12>
       A710-IO.                                                         <RC002>
      *                                                                 <RC002>
      *  If Modify:                                                     <RC002>
      *  Read the ZPPR with Receipt Number.                             <RC002>
      *    If Receipt Number already existed, update with new infos.    <RC002>
      *    If not existed, Create new record in ZPPR.                   <RC002>
      *                                                                 <RC002>
           MOVE SPACE              TO ZPPR-PARAMS      .                <RC002>
           MOVE PRFX-CASH          TO ZPPR-PREFIX      .                <RC002>
           MOVE WSSP-COMPANY       TO ZPPR-COMPANY     .                <RC002>
           MOVE SPACES             TO ZPPR-RCPREPRTCD  .                <RC002>
           MOVE SPACES             TO ZPPR-RCPREPRNT   .                <RC002>
           MOVE S2610-DOCNO        TO ZPPR-RECEIPT     .                <RC002>
           MOVE WSSP-BANKCODE      TO ZPPR-BANKCODE    .                <RC002>
           MOVE '1'                TO ZPPR-VALIDFLAG   .                <RC002>
                                                                        <RC002>
           MOVE ZPPRREC            TO ZPPR-FORMAT      .                <RC002>
           MOVE BEGNH              TO ZPPR-FUNCTION    .                <RC002>
                                                                        <RC002>
       A720-CALL-ZPPRIO.                                                <RC002>
                                                                        <RC002>
           CALL 'ZPPRIO'        USING ZPPR-PARAMS      .                <RC002>
                                                                        <RC002>
           IF ZPPR-STATUZ       NOT = O-K AND ENDP                      <RC002>
               MOVE ZPPR-STATUZ         TO SYSR-STATUZ                  <RC002>
               MOVE ZPPR-PARAMS         TO SYSR-PARAMS                  <RC002>
               PERFORM 600-FATAL-ERROR                                  <RC002>
           END-IF.                                                      <RC002>
                                                                        <RC002>
           IF  ZPPR-STATUZ          = ENDP                              <RC002>
           OR  PRFX-CASH        NOT = ZPPR-PREFIX  OR                   <RC002>
               WSSP-COMPANY     NOT = ZPPR-COMPANY  OR                  <RC002>
               S2610-DOCNO      NOT = ZPPR-RECEIPT  OR                  <RC002>
               WSSP-BANKCODE    NOT = ZPPR-BANKCODE OR                  <RC002>
               ZPPR-VALIDFLAG   NOT = '1'                               <RC002>
               MOVE RLSE           TO  ZPPR-FUNCTION                    <RC002>
                                                                        <RC002>
               CALL 'ZPPRIO'        USING ZPPR-PARAMS                   <RC002>
                                                                        <RC002>
               IF ZPPR-STATUZ       NOT = O-K                           <RC002>
                   MOVE ZPPR-STATUZ         TO SYSR-STATUZ              <RC002>
                   MOVE ZPPR-PARAMS         TO SYSR-PARAMS              <RC002>
                   PERFORM 600-FATAL-ERROR                              <RC002>
               END-IF                                                   <RC002>
               GO TO A719-EXIT                                          <RC002>
           END-IF.                                                      <RC002>
      *  If existed                                                     <RC002>
           IF ZPPR-STATUZ           = O-K                               <RC002>
           AND PRFX-CASH            =  ZPPR-PREFIX                      <RC002>
           AND WSSP-COMPANY         =  ZPPR-COMPANY                     <RC002>
           AND S2610-DOCNO          =  ZPPR-RECEIPT                     <RC002>
           AND WSSP-BANKCODE        =  ZPPR-BANKCODE                    <RC002>
           AND ZPPR-VALIDFLAG       =  '1'                              <RC002>
      *     -----> Just Update current record with new infos            <RC002>
      *        MOVE SR289-RCPREPRNT    TO ZPPR-RCPREPRNT                <RC002>
      *        MOVE SR289-RCPREPRTCD   TO ZPPR-RCPREPRTCD               <RC002>
               MOVE WSSP-SERVUNIT      TO ZPPR-RCPREPRTCD               <RC002>
               MOVE WSSP-COMMON-AREA(102:8)                             <RC002>
                                       TO ZPPR-RCPREPRNT                <RC002>
               MOVE REWRT              TO ZPPR-FUNCTION                 <RC002>
                                                                        <RC002>
               CALL 'ZPPRIO'        USING ZPPR-PARAMS                   <RC002>
                                                                        <RC002>
               IF ZPPR-STATUZ       NOT = O-K                           <RC002>
                   MOVE ZPPR-STATUZ         TO SYSR-STATUZ              <RC002>
                   MOVE ZPPR-PARAMS         TO SYSR-PARAMS              <RC002>
                   PERFORM 600-FATAL-ERROR                              <RC002>
               END-IF                                                   <RC002>
           ELSE                                                         <RC002>
      *        Not existed --> release the records then create new      <RC002>
               MOVE RLSE           TO ZPPR-FUNCTION                     <RC002>
               CALL 'ZPPRIO'        USING ZPPR-PARAMS                   <RC002>
                                                                        <RC002>
               IF ZPPR-STATUZ       NOT = O-K                           <RC002>
                   MOVE ZPPR-STATUZ         TO SYSR-STATUZ              <RC002>
                   MOVE ZPPR-PARAMS         TO SYSR-PARAMS              <RC002>
                   PERFORM 600-FATAL-ERROR                              <RC002>
               END-IF                                                   <RC002>
                                                                        <RC002>
               MOVE SPACE              TO ZPPR-PARAMS                   <RC002>
               MOVE PRFX-CASH          TO ZPPR-PREFIX                   <RC002>
               MOVE WSSP-COMPANY       TO ZPPR-COMPANY                  <RC002>
               MOVE WSSP-SERVUNIT      TO ZPPR-RCPREPRTCD               <RC002>
               MOVE WSSP-COMMON-AREA(102:8)                             <RC002>
                                   TO ZPPR-RCPREPRNT                    <RC002>
               MOVE S2610-DOCNO        TO ZPPR-RECEIPT                  <RC002>
               MOVE WSSP-BANKCODE      TO ZPPR-BANKCODE                 <RC002>
               MOVE '1'                TO ZPPR-VALIDFLAG                <RC002>
                                                                        <RC002>
               MOVE ZPPRREC            TO ZPPR-FORMAT                   <RC002>
               MOVE WRITR              TO ZPPR-FUNCTION                 <RC002>
                                                                        <RC002>
               CALL 'ZPPRIO'        USING ZPPR-PARAMS                   <RC002>
                                                                        <RC002>
               IF ZPPR-STATUZ       NOT = O-K                           <RC002>
                   MOVE ZPPR-STATUZ         TO SYSR-STATUZ              <RC002>
                   MOVE ZPPR-PARAMS         TO SYSR-PARAMS              <RC002>
                   PERFORM 600-FATAL-ERROR                              <RC002>
               END-IF                                                   <RC002>
           END-IF.                                                      <RC002>
       A719-EXIT.                                                       <RC002>
           EXIT.                                                        <RC002>
      /                                                                 <PS008>
       A800-UPDATE-CLRR-USEED SECTION.                                  <CS016>
      *********************************                                 <CS016>
       A801-START.                                                      <CS016>
      *                                                                 <CS016>
           INITIALIZE                  CLRRENQ-PARAMS.                  <CS016>
           MOVE 'CA'                   TO CLRRENQ-FOREPFX.              <CS016>
           MOVE '2'                    TO CLRRENQ-FORECOY.              <CS016>
           MOVE S2610-DOCNO            TO CLRRENQ-FORENUM.              <CS016>
           MOVE 'CF'                   TO CLRRENQ-CLRRROLE.             <CS016>
           MOVE CLRRENQREC             TO CLRRENQ-FORMAT.               <CS016>
           MOVE READR                  TO CLRRENQ-FUNCTION.             <CS016>
                                                                        <CS016>
           CALL 'CLRRENQIO'            USING CLRRENQ-PARAMS.            <CS016>
                                                                        <CS016>
           IF CLRRENQ-STATUZ           NOT = O-K                        <CS016>
           AND                         NOT = MRNF                       <CS016>
               MOVE CLRRENQ-PARAMS     TO SYSR-PARAMS                   <CS016>
               PERFORM 600-FATAL-ERROR                                  <CS016>
           END-IF.                                                      <CS016>
      *                                                                 <CS016>
       A809-EXIT.                                                       <CS016>
           EXIT.                                                        <CS016>
      /                                                                 <CS016>
       A900-READ-RTRN SECTION.                                          <CS016>
      **************************                                        <CS016>
       A901-START.                                                      <CS016>
      *                                                                 <CS016>
                                                                        <CS016>
           INITIALIZE                     RTRNDOC-PARAMS.               <CS016>
           MOVE S2610-DOCNO            TO RTRNDOC-RDOCNUM.              <CS016>
           MOVE WSAA-CODE              TO RTRNDOC-SACSCODE.             <CS016>
           MOVE WSAA-TYPE              TO RTRNDOC-SACSTYP.              <CS016>
           MOVE RTRNDOCREC             TO RTRNDOC-FORMAT.               <CS016>
           MOVE READR                  TO RTRNDOC-FUNCTION.             <CS016>
                                                                        <CS016>
           CALL 'RTRNDOCIO'            USING RTRNDOC-PARAMS.            <CS016>
                                                                        <CS016>
           IF  RTRNDOC-STATUZ          NOT = O-K                        <CS016>
           AND                         NOT = MRNF                       <CS016>
               MOVE RTRNDOC-STATUZ     TO SYSR-STATUZ                   <CS016>
               MOVE RTRNDOC-PARAMS     TO SYSR-PARAMS                   <CS016>
               PERFORM 600-FATAL-ERROR                                  <CS016>
           END-IF.                                                      <CS016>
                                                                        <CS016>
           IF RTRNDOC-STATUZ           = O-K                            <CS016>
              MOVE RTRNDOC-BATCTRCDE   TO WSAA-TRANCODE                 <CS016>
           END-IF.                                                      <CS016>
      *                                                                 <CS016>
       A909-EXIT.                                                       <CS016>
           EXIT.                                                        <CS016>
      /                                                                 <CS016>
       B900-LOOP-SUBFILE-UPDATE SECTION.                                <CS016>
      ***************************                                       <CS016>
       B901-START.                                                      <CS016>
      *                                                                 <CS016>
           IF  S2610-SACSCODE          = SPACES                         <PHFX54>
           OR  S2610-SACSTYPW          = SPACES                         <PHFX54>
               MOVE ENDP               TO SCRN-STATUZ                   <PHFX54>
               GO TO B909-EXIT                                          <PHFX54>
           END-IF.                                                      <PHFX54>
                                                                        <PHFX54>
           MOVE S2610-SACSCODE         TO WSAA-CODE.                    <CS016>
           MOVE S2610-SACSTYPW         TO WSAA-TYPE.                    <CS016>
           PERFORM A900-READ-RTRN.                                      <CS016>
                                                                        <CS016>
      **** IF RTRNDOC-STATUZ           = O-K                    <PHFX54><CS016>
      **** AND RTRNDOC-TRANDESC        NOT = S2610-TRANDESC     <PHFX54><CS016>
      ****    MOVE S2610-TRANDESC      TO RTRNDOC-TRANDESC      <PHFX54><CS016>
      ****    PERFORM C900-UPDATE-RTRN                          <PHFX54><CS016>
      **** END-IF.                                              <PHFX54><CS016>
                                                                        <CS016>
           IF RTRNDOC-STATUZ           = O-K                            <PHFX54>
           AND RTRNDOC-RDOCNUM         = S2610-DOCNO                    <PHFX54>
              IF RTRNDOC-TRANDESC  NOT = S2610-TRANDESC                 <PHFX54>
                 MOVE S2610-TRANDESC   TO RTRNDOC-TRANDESC              <PHFX54>
              END-IF                                                    <PHFX54>
              IF RTRNDOC-EFFDATE   NOT = WSSP-TRANDATE                  <PHFX54>
                 MOVE WSSP-TRANDATE TO RTRNDOC-EFFDATE                  <PHFX54>
              END-IF                                                    <PHFX54>
              PERFORM C900-UPDATE-RTRN                                  <PHFX54>
           END-IF.                                                      <PHFX54>
                                                                        <PHFX54>
           MOVE SRDN                   TO SCRN-FUNCTION.                <CS016>
           PERFORM 9000-S2610-IO.                                       <CS016>
      *                                                                 <CS016>
       B909-EXIT.                                                       <CS016>
           EXIT.                                                        <CS016>
      /                                                                 <CS016>
       C900-UPDATE-RTRN SECTION.                                        <CS016>
      **************************                                        <CS016>
       C901-START.                                                      <CS016>
      *                                                                 <CS016>
           MOVE WRITD                  TO RTRNDOC-FUNCTION.             <CS016>
                                                                        <CS016>
           CALL 'RTRNDOCIO'            USING RTRNDOC-PARAMS.            <CS016>
                                                                        <CS016>
           IF  RTRNDOC-STATUZ          NOT = O-K                        <CS016>
               MOVE RTRNDOC-STATUZ     TO SYSR-STATUZ                   <CS016>
               MOVE RTRNDOC-PARAMS     TO SYSR-PARAMS                   <CS016>
               PERFORM 600-FATAL-ERROR                                  <CS016>
           END-IF.                                                      <CS016>

           MOVE 'Y'                    TO WSAA-UPDATE-FLAG.
      *                                                                 <CS016>
       C909-EXIT.                                                       <CS016>
           EXIT.                                                        <CS016>
      /                                                                 <CS016>
