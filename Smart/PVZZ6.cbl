      * Generation Parameters - SCRVER(02)            Do Not Delete|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PVZZ6.
      *
      * Copyright 1986-2014, Computer Sciences Corporation.
      *
      *
      *REMARKS.
      *
      * The remarks section should detail the main processes of the
      * program and any special functions.
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      **DD/MM/YY*************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'PVZZ6'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
       01  WSAA-EXIST-STAFF            PIC X(01).
      *
       01  ERRORS.
           03  E005                    PIC X(04) VALUE 'E005'.
           03  B369                    PIC X(04) VALUE 'B369'.
      *
       01  TABLES.
           03  TNNNN                   PIC X(05) VALUE 'TNNNN'.
      *
       01  FORMATS.
           03  XXXXREC                 PIC X(10) VALUE 'XXXXREC'.
           03  ZZZ4REC                 PIC X(10) VALUE 'ZZZ4REC'.
      /
           COPY OPSTATSREC.
      /
           COPY SYSERRREC.
      /
           COPY VARCOM.
      /
       LINKAGE SECTION.

           COPY WSSPCOMN.
      /
       01  WSSP-USER-AREA              PIC X(768).
      /
           COPY SCRNPARAMS.
      /
           COPY SVZZ6SCR.
      /
       PROCEDURE DIVISION           USING WSSP-COMMON-AREA
                                          WSSP-USER-AREA
                                          SCRN-SCREEN-PARAMS
                                          SVZZ6-DATA-AREA.

           COPY MAING.
      /
      *****************************************************************
      *      Initialise Fields for showing on Screen
      *****************************************************************
      *
       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.
      *
      *  Initialise Fields
      *
           MOVE SPACES                 TO SVZZ6-DATA-AREA.
           MOVE 'Y'                    TO WSAA-EXIST-STAFF.

      *    Dummy field initialisation for prototype version.
******

      *
      *    Set screen fields
      *

      *
       1090-EXIT.
            EXIT.
      /
      *
      *    Sections performed from the 1000 section above.
      *
      /
       PRE-SCREEN-EDIT SECTION.
      *************************
      *
       PRE-START.
      *---------------------------------------------------------------*
      *    This section will handle any action required on the screen *
      *    before the screen is painted.                              *
      *---------------------------------------------------------------*
            GO TO PRE-EXIT.
      *
       PRE-EXIT.
           EXIT.
      /
      *****************************************************************
      *     Retrieve Screen Fields and Edit
      *****************************************************************
      *
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2010-SCREEN-IO.
      *
      *  Perform Screen Validation
      *
           MOVE O-K                    TO WSSP-EDTERROR.
      *
       2020-VALIDATE.
      *

      *
      *    Validate fields
      *

      *
       2080-CHECK-FOR-ERRORS.
      *
           IF  SVZZ6-ERROR-INDICATORS
                                    NOT = SPACES
               MOVE 'Y'                TO WSSP-EDTERROR
           END-IF.
      *
       2090-EXIT.
            EXIT.
      /

      *
      *    Sections performed from the 2000 section above.
      *
       2100-CHECK-Exist-STAFF SECTION.
       2110-START.

           MOVE SVZZ6-AGENTNO          TO ZZZ4-TAGNTNUM.
           MOVE ZZZ4REC                TO ZZZ4-FORMAT.
           MOVE READR                  TO ZZZ4-FUNCTION.

           CALL 'ZZZ4IO'               USING ZZZ4-PARAMS.

           IF ZZZ4-STATUZ              NOT = O-K
                                       AND NOT = MRNF

                MOVE ZZZ4-PARAMS       TO SYSR-PARAMS
                PERFORM 600-FATAL-ERROR

           END-IF.

           IF ZZZ4-STATUZ              = MRNF

               MOVE 'N'                TO WSAA-EXIST-STAFF
               GO TO 2050-EXIT

           ELSE 

               MOVE 'Y'                TO WSAA-EXIST-STAFF
               MOVE SPACES             TO WSSP-EDTERROR
               MOVE B369               TO SVZZ6-AGENTNO-ERR.
               GO TO 2050-EXIT.

           END-IF                                   



       21 50-EXIT.
           EXIT.
      /
      *****************************************************************
      *     Update Database if required and Log Transaction
      *****************************************************************
      *
       3000-UPDATE SECTION.
      *********************
      *
       3010-UPDATE-DATABASE.
      *
           IF WSSP-FLAG                = 'A'

               PERFORM 3100-ASIGN-STAFF-PROPERTIES
               PERFORM 3200-INSERT-NEW-STAFF

           END-IF.    

      *
      *  Update database files as required / WSSP
      *

       3090-EXIT.
            EXIT.

      /

      *
      *    Sections performed from the 3000 section above.
      *
       
       3100-ASIGN-STAFF-PROPERTIES SECTION

       3110-START.

           MOVE SVZZ6-AGENTNO          TO ZZZ4-TAGNTNUM.
           MOVE SVZZ6-CLNTNAM          TO ZZZ4-TLNAME.
           MOVE SVZZ6-GIVNAME          TO ZZZ4-TFNAME.
           MOVE SVZZ6-OFFCDE           TO ZZZ4-TAREACODE.

       3150-EXIT.
           EXIT.
       
       
       3200-INSERT-NEW-STAFF SECTION.
       3210-START.
           
           MOVE WRITR                  TO ZZZ4-FUNCTION            
           MOVE ZZZ4REC                TO ZZZ4-FORMAT              
           CALL 'ZZZ4IO'               USING ZZZ4-PARAMS           
           IF ZZZ4-STATUZ              NOT = O-K                   
                                                   
               MOVE ZZZ4-PARAMS       TO    SYSR-PARAMS           
               PERFORM 600-FATAL-ERROR                         
                                                   
           END-IF.                                            
           
       3250-EXIT.
           EXIT.


      /
      *****************************************************************
      *     Decide which Transaction Program is Next
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.
      *
      *  Set Next Program
      *
           ADD 1                       TO WSSP-PROGRAM-PTR.
      *
      *
       4090-EXIT.
            EXIT.
