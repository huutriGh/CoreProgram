      * Generation Parameters - SCRVER(02)            Do Not Delete|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PVZZ2.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *
      *REMARKS.
      *            PVZZ2 Mainline.
      *
      *   Parameter prompt program
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 16/11/20  01/01   DUMMY        IT-Nguyen Huu Tri                    *
      *           VALIDATE FIELD STATUS SELECTED                            *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                IBM-AS400.
       OBJECT-COMPUTER.                IBM-AS400.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'PVZZ2'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.

       01  ERRORS.
           03  A123                    PIC X(04) VALUE 'A123'.
           03  E186                    PIC X(04) VALUE 'E186'.
           03  F409                    PIC X(04) VALUE 'F409'.

       01  FORMATS.
           03  BPARREC                 PIC X(10) VALUE 'BPARREC'.
           03  BPPDREC                 PIC X(10) VALUE 'BPPDREC'.
           03  BSCDREC                 PIC X(10) VALUE 'BSCDREC'.
           03  BSSCREC                 PIC X(10) VALUE 'BSSCREC'.
           03  BUPAREC                 PIC X(10) VALUE 'BUPAREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.

      *
       01  WSAA-TIME.
           03  WSAA-HHMMSS             PIC 9(06).
           03  FILLER                  PIC X(02).

       01  WSAA-BATCKEY.
           COPY BATCKEY.
      /
           COPY VARCOM.
      /
           COPY DATCON1REC.

           COPY SPCOUTREC.

           COPY SYSERRREC.
      /
           COPY PVZZ2PAR.
      /
           COPY WSSPREC.
      /
           COPY BSCDSKM.
      /
           COPY BSSCSKM.
      /
           COPY BUPASKM.
      /
           COPY BPPDSKM.
      /
           COPY BPARSKM.
      /
           COPY SUBJOBREC.
      /
           COPY OPSTATSREC.
      /
           COPY ITEMSKM.
      /
           COPY DESCSKM.
      /
           COPY TVZZ1REC.
      /
       LINKAGE SECTION.

           COPY WSSPCOMN.

           COPY WSSPSMART.

           COPY SCRNPARAMS.

           COPY SVZZ2SCR.
      /
       PROCEDURE DIVISION           USING WSSP-COMMON-AREA
                                          WSSP-USER-AREA
                                          SCRN-SCREEN-PARAMS
                                          SVZZ2-DATA-AREA.

           COPY MAING.
      /
      *****************************************************************
      *      Initialise Fields for Showing on Screen
      *****************************************************************

       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.
      *
      * Retrieve Schedule.
      *
           MOVE BSSCREC                TO BSSC-FORMAT.
           MOVE RETRV                  TO BSSC-FUNCTION.

           CALL 'BSSCIO'            USING BSSC-PARAMS.

           IF  BSSC-STATUZ          NOT = O-K
               MOVE BSSC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
      * Retrieve Schedule Definition.
      *
           MOVE BSCDREC                TO BSCD-FORMAT.
           MOVE RETRV                  TO BSCD-FUNCTION.

           CALL 'BSCDIO'            USING BSCD-PARAMS.

           IF  BSCD-STATUZ          NOT = O-K
               MOVE BSCD-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
      * Retrieve Parameter Prompt Definition.
      *
           MOVE BPPDREC                TO BPPD-FORMAT.
           MOVE RETRV                  TO BPPD-FUNCTION.

           CALL 'BPPDIO'            USING BPPD-PARAMS.

           IF  BPPD-STATUZ          NOT = O-K
               MOVE BPPD-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

      *****************************************************************

           MOVE SPACES                 TO SVZZ2-DATA-AREA.



           MOVE BSSC-SCHEDULE-NAME     TO SVZZ2-SCHEDULE-NAME.
           MOVE BSSC-SCHEDULE-NUMBER   TO SVZZ2-SCHEDULE-NUMBER.
           MOVE BSSC-EFFECTIVE-DATE    TO SVZZ2-EFFDATE.
           MOVE BSSC-ACCT-MONTH        TO SVZZ2-ACCTMONTH.
           MOVE BSSC-ACCT-YEAR         TO SVZZ2-ACCTYEAR.
           MOVE BSCD-JOBQ              TO SVZZ2-JOBQ.
           MOVE BPPD-COMPANY           TO SVZZ2-BCOMPANY.
           MOVE BSSC-INIT-BRANCH       TO SVZZ2-BBRANCH.

           IF  WSSP-FLAG            NOT = 'I'
           AND WSSP-FLAG            NOT = 'M'
               GO TO 1090-EXIT
           END-IF.

           INITIALIZE                     BPAR-PARAMS.
           MOVE BSSC-SCHEDULE-NAME     TO BPAR-SCHEDULE-NAME.
           MOVE BPPD-COMPANY           TO BPAR-COMPANY.
           MOVE WSAA-PROG              TO BPAR-PARM-PROMPT-PROG.
           MOVE WSSP-INQKEY(1:8)       TO BPAR-BRUNTYPE.
           MOVE WSSP-INQKEY(9:3)       TO BPAR-BRUNOCCUR.
           MOVE BSSC-EFFECTIVE-DATE    TO BPAR-EFFECTIVE-DATE.
           MOVE BSSC-ACCT-MONTH        TO BPAR-ACCTMONTH.
           MOVE BSSC-ACCT-YEAR         TO BPAR-ACCTYEAR.

           MOVE READR                  TO BPAR-FUNCTION.
           MOVE BPARREC                TO BPAR-FORMAT.

           CALL 'BPARIO'            USING BPAR-PARAMS.

           IF  BPAR-STATUZ          NOT = O-K
           AND BPAR-STATUZ          NOT = MRNF
               MOVE BPAR-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
           IF  BPAR-STATUZ              = MRNF
               MOVE A123               TO SCRN-ERROR-CODE
               GO TO 1090-EXIT
           END-IF.
      *
           MOVE BPAR-PARMAREA          TO PVZZ2-PARM-RECORD.
      *
           MOVE PVZZ2-ACCTDESC         TO SVZZ2-ACCTDESC       .
           MOVE PVZZ2-ACCTYP           TO SVZZ2-ACCTYP         .
           MOVE PVZZ2-BUSDSC           TO SVZZ2-BUSDSC         .

      *
       1090-EXIT.
            EXIT.
      /
       PRE-SCREEN-EDIT SECTION.
      *************************
      *
       PRE-START.
      *---------------------------------------------------------------*
      *    This section will handle any action required on the screen *
      *    before the screen is painted.                              *
      *---------------------------------------------------------------*
            IF  WSSP-FLAG               = 'I'
                MOVE PROT              TO SCRN-FUNCTION
            END-IF.

            GO TO PRE-EXIT.
      *
       PRE-EXIT.
           EXIT.
      /
      *****************************************************************
      *     Retrieve Screen Fields and Edit
      *****************************************************************

       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2010-SCREEN-IO.
      *
      * Screen Validation
      *
           MOVE O-K                    TO WSSP-EDTERROR.

           IF  WSSP-FLAG                = 'I'
               GO TO 2090-EXIT
           END-IF.
      *
       2020-VALIDATE.
      *
           IF SCRN-STATUZ               = 'CALC'
               MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.

      *
      *    Validate fields
      *
           IF SVZZ2-ACCTYP               = SPACES
               MOVE E186                 TO SVZZ2-ACCTYP-ERR
               GO TO 2080-CHECK-FOR-ERRORS
           END-IF.

           PERFORM 2100-READ-TVZZ1-TABLE.

      *
       2080-CHECK-FOR-ERRORS.
      *
           IF  SVZZ2-ERROR-INDICATORS
                                    NOT = SPACES
               MOVE 'Y'                TO WSSP-EDTERROR
           END-IF.
      *
       2090-EXIT.
            EXIT.
      /
       2100-READ-TVZZ1-TABLE SECTION.
      *******************************
       2110-START-READ-TVZZ1-ON-ITEM.
           MOVE SVZZ2-ACCTYP             TO ITEM-ITEMITEM.
           MOVE 'TVZZ1'                  TO ITEM-ITEMTABL.
           MOVE 'IT'                     TO ITEM-ITEMPFX.
           MOVE '2'                      TO ITEM-ITEMCOY.
           MOVE SPACE                    TO ITEM-ITEMSEQ.
           MOVE READR                    TO ITEM-FUNCTION.
           MOVE ITEMREC                  TO ITEM-FORMAT.

           CALL 'ITEMIO'                 USING ITEM-PARAMS.

           IF ITEM-STATUZ                NOT = O-K
              AND ITEM-STATUZ            NOT = MRNF

                  MOVE ITEM-PARAMS        TO SYSR-PARAMS
                  PERFORM 600-FATAL-ERROR
           END-IF.

           IF ITEM-STATUZ                 = MRNF
              MOVE F409                  TO SVZZ2-ACCTYP-ERR
           ELSE
              MOVE ITEM-GENAREA          TO TVZZ1-TVZZ1-REC
              MOVE TVZZ1-DESC            TO SVZZ2-BUSDSC
              PERFORM 2200-READ-TVZZ1-TABLE-ON-DESC
           END-IF.
       2150-EXIT.
            EXIT.
      /
       2200-READ-TVZZ1-TABLE-ON-DESC SECTION.
      ***************************************
       2210-START-READ-TVZZ1-ON-DESC.
           MOVE 'IT'                     TO DESC-DESCPFX.
           MOVE '2'                      TO DESC-DESCCOY.
           MOVE 'TVZZ1'                  TO DESC-DESCTABL.
           MOVE SVZZ2-ACCTYP             TO DESC-DESCITEM.
           MOVE SPACE                    TO DESC-ITEMSEQ.
           MOVE 'E'                      TO DESC-LANGUAGE.
           MOVE READR                    TO DESC-FUNCTION.
           MOVE DESCREC                  TO DESC-FORMAT.


           CALL 'DESCIO'                 USING DESC-PARAMS.

           IF DESC-STATUZ                NOT = O-K
              AND DESC-STATUZ            NOT = MRNF
                  MOVE DESC-PARAMS        TO SYSR-PARAMS
                  PERFORM 600-FATAL-ERROR
           END-IF.

           IF DESC-STATUZ                 = MRNF
              MOVE F409                  TO SVZZ2-ACCTYP-ERR
           ELSE
              MOVE DESC-LONGDESC         TO SVZZ2-ACCTDESC
           END-IF.

       2250-EXIT.
            EXIT.
      *
      *    Sections performed from the 2000 section above.
      *

      /
      *****************************************************************
      *     Update Database if Required and Log Transaction
      *****************************************************************

       3000-UPDATE SECTION.
      *********************
      *
       3010-LOAD-FIELDS.
      *
      * Set up the Parameter Record
      *
           PERFORM 3100-MOVE-PARAMETERS.
           PERFORM 3200-WRITE-PARAM-RECORD.
      *
       3090-EXIT.
            EXIT.
      /
      *****************************************************************
      *     Updating Sections
      *****************************************************************

       3100-MOVE-PARAMETERS SECTION.
      ******************************
      *
       3110-MOVE-TO-PARM-RECORD.
      *
      * Set up Parameter Record
      *
           MOVE SVZZ2-ACCTDESC         TO PVZZ2-ACCTDESC       .
           MOVE SVZZ2-ACCTYP           TO PVZZ2-ACCTYP         .
           MOVE SVZZ2-BUSDSC           TO PVZZ2-BUSDSC         .

      *
       3190-EXIT.
           EXIT.
      /
       3200-WRITE-PARAM-RECORD SECTION.
      *********************************
      *
       3210-WRIT-BUPA.
      *
      * Write BUPA Record
      *
           MOVE SPACES                 TO BUPA-PARAMS.
           MOVE SVZZ2-SCHEDULE-NAME    TO BUPA-SCHEDULE-NAME.
           MOVE SVZZ2-SCHEDULE-NUMBER  TO BUPA-SCHEDULE-NUMBER.
           MOVE SVZZ2-EFFDATE          TO BUPA-EFFECTIVE-DATE.
           MOVE SVZZ2-ACCTMONTH        TO BUPA-ACCT-MONTH.
           MOVE SVZZ2-ACCTYEAR         TO BUPA-ACCT-YEAR.
           MOVE SVZZ2-BCOMPANY         TO BUPA-COMPANY.
           MOVE SVZZ2-BBRANCH          TO BUPA-BRANCH.

           MOVE PVZZ2-PARM-RECORD      TO BUPA-PARMAREA.

           MOVE BPPD-PARM-PROMPT-PROG  TO BUPA-PARM-PROMPT-PROG.

           MOVE BUPAREC                TO BUPA-FORMAT.
           MOVE WRITR                  TO BUPA-FUNCTION.

           CALL 'BUPAIO'            USING BUPA-PARAMS.

           IF  BUPA-STATUZ          NOT = O-K
               MOVE BUPA-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       3290-EXIT.
           EXIT.
      /
      *****************************************************************
      *     Decide which Transaction Program is Next
      *****************************************************************

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