      * Generation Parameters - SCRVER(02)            Do Not Delete|
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                                         P2635.
      *
      *(C) Copyright CSC Corporation Limited 1986 - 1999.
      *    All rights reserved. CSC Confidential.
      *
      *REMARKS.
      * REPLACE BY TABLE DESCRIPTION.
      *
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
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
       01  WSAA-PROG                   PIC X(05) VALUE 'P2635'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
       01  WSAA-UPDATE-FLAG            PIC X(01).
       01  WSAA-FIRST-TIME             PIC X(01) VALUE 'Y'.
       01  WSAA-SEQ                    PIC 9(02).
       01  WSAA-STORE-SEQNO            PIC X(02).
       01  WSAA-TABLISTREC             PIC X(575).
      *
       01  ERRORS.
           03  E026                    PIC X(04) VALUE 'E026'.
           03  E027                    PIC X(04) VALUE 'E027'.
           03  E186                    PIC X(04) VALUE 'E186'.
      *
       01  TABLES.
           03  T2635                   PIC X(05) VALUE 'T2635'.
      *
       01  FORMATS.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
      /
           COPY VARCOM.
      *
           COPY SYSERRREC.
      *
           COPY OPSTATSREC.
      /
           COPY T2635REC.
      /
           COPY ITEMSKM.
      /
           COPY DESCSKM.
      /
       LINKAGE SECTION.
      *
           COPY WSSPCOMN.
      *
           COPY WSSPSMART.
      /
           COPY SCRNPARAMS.
      *
           COPY S2635SCR.
      /
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA
                                SCRN-SCREEN-PARAMS S2635-DATA-AREA.
      *
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
           IF WSAA-FIRST-TIME          NOT = 'Y'
              GO TO 1020-READ-PRIMARY-RECORD
           END-IF.

           IF ITEM-ITEMSEQ             = SPACES
              MOVE ZERO                TO WSAA-SEQ
           ELSE
              MOVE ITEM-ITEMSEQ        TO WSAA-SEQ
           END-IF.
      *
       1015-INITIALISE-SCREEN.
      *
           MOVE SPACES                 TO S2635-DATA-AREA.
      *
       1020-READ-PRIMARY-RECORD.
      *
           MOVE WSSP-ITEMKEY           TO ITEM-DATA-KEY.
           IF WSAA-SEQ                 = ZERO
              MOVE SPACE               TO ITEM-ITEMSEQ
           ELSE
              MOVE WSAA-SEQ            TO ITEM-ITEMSEQ
           END-IF.
      *
       1025-READ-RECORD.
      *
           MOVE READR                  TO ITEM-FUNCTION.

           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF ITEM-STATUZ              NOT = O-K
                                   AND NOT = MRNF
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
       1030-READ-SECONDARY-RECORDS.
      *
      *****IF WSAA-FIRST-TIME          NOT = 'Y'
      *****   GO TO 1040-MOVE-TO-SCREEN
      *****END-IF.
      *
       1031-READ-RECORD.
      *
           MOVE SPACES                 TO DESC-PARAMS.
           MOVE ITEM-ITEMPFX           TO DESC-DESCPFX.
           MOVE ITEM-ITEMCOY           TO DESC-DESCCOY.
           MOVE ITEM-ITEMTABL          TO DESC-DESCTABL.
           MOVE ITEM-ITEMITEM          TO DESC-DESCITEM.
           MOVE SPACES                 TO DESC-ITEMSEQ.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.

           CALL 'DESCIO' USING DESC-PARAMS.

           IF DESC-STATUZ              NOT = O-K
                                   AND NOT = MRNF
              MOVE DESC-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE 'N'                    TO WSAA-FIRST-TIME.
      *
       1040-MOVE-TO-SCREEN.
      *
           IF ITEM-STATUZ              = MRNF
           AND ITEM-ITEMSEQ            = SPACES
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF ITEM-STATUZ              = MRNF
           AND WSSP-FLAG               = 'I'
              MOVE E026                TO SCRN-ERROR-CODE
              SUBTRACT 1               FROM WSAA-SEQ
              GO TO 1080-OTHER
           END-IF.

           IF ITEM-STATUZ              = MRNF
              MOVE SPACES              TO ITEM-GENAREA
           END-IF.

           MOVE ITEM-ITEMCOY           TO S2635-COMPANY.
           MOVE ITEM-ITEMTABL          TO S2635-TABL.
           MOVE ITEM-ITEMITEM          TO S2635-ITEM.

           MOVE DESC-LONGDESC          TO S2635-LONGDESC.

           IF DESC-STATUZ              = MRNF
              MOVE SPACES              TO S2635-LONGDESC
           END-IF.

           MOVE ITEM-GENAREA           TO T2635-T2635-REC.
      *
      *---------------------------------------------------------------*
      *    IF THE EXTRA DATA AREA WAS NOT USED BEFORE THE NUMERIC     *
      *    FIELDS MUST BE SET TO ZERO TO AVOID DATA EXCEPTIONS.       *
      *---------------------------------------------------------------*
      *
           IF ITEM-GENAREA             NOT = SPACES
              GO TO 1045-GENERAL-AREA
           END-IF.
      *

      *
       1045-GENERAL-AREA.
      *
           MOVE T2635-FLDIDS
             TO S2635-FLDIDS                  .

      *
       1050-CONFIRMATION-FIELDS.
      *
      *
       1080-OTHER.
      *
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
           IF WSSP-FLAG                = 'I'
              MOVE PROT                TO SCRN-FUNCTION
           END-IF.

           GO TO PRE-EXIT.
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
           MOVE O-K                    TO WSSP-EDTERROR.
      *
       2020-VALIDATE.
      *
           IF SCRN-STATUZ              = ROLD
           AND ITEM-ITEMSEQ            = SPACES
              MOVE E027                TO SCRN-ERROR-CODE
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.

           IF SCRN-STATUZ              = ROLU
           AND ITEM-ITEMSEQ            = '99'
              MOVE E026                TO SCRN-ERROR-CODE
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.

           IF WSSP-FLAG                = 'I'
              GO TO 2090-EXIT
           END-IF.

      *---------------------------------------------------------------*
      *    Enter screen validation here.                              *
      *---------------------------------------------------------------*

      *
       2080-OTHER.
      *
       2090-EXIT.
      *
           IF S2635-ERROR-INDICATORS   NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.
      *
       2099-EXIT.
           EXIT.
      /
      *****************************************************************
      *    UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3010-PREPARATION.
      *
           IF WSSP-FLAG                = 'I'
              GO TO 3090-EXIT
           END-IF.
      *
       3030-CHECK-CHANGES.
      *
           MOVE 'N'                    TO WSAA-UPDATE-FLAG.

           IF  WSSP-FLAG               = 'C'
               MOVE 'Y'                TO WSAA-UPDATE-FLAG
           END-IF.

           PERFORM 3100-CHECK-CHANGES.

           IF WSAA-UPDATE-FLAG         NOT = 'Y'
              GO TO 3080-OTHER
           END-IF.
      *
       3050-UPDATE-PRIMARY-RECORD.
      *
           MOVE READH                  TO ITEM-FUNCTION.
           MOVE WSSP-ITEMKEY           TO ITEM-DATA-KEY.
           IF WSAA-SEQ = ZERO
              MOVE SPACE               TO ITEM-ITEMSEQ
           ELSE
              MOVE WSAA-SEQ            TO ITEM-ITEMSEQ
           END-IF.

           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF ITEM-STATUZ              NOT = O-K
                                   AND NOT = MRNF
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE WSSP-TRANID            TO VRCM-TRANID.
           MOVE VRCM-TERMID            TO VRCM-COMP-TERMID.
           MOVE VRCM-TRANID-N          TO VRCM-COMP-TRANID-N.
           MOVE VRCM-COMP-TRANID       TO ITEM-TRANID.
      *
       3055-UPDATE-RECORD.
      *
           MOVE WSAA-PROG              TO ITEM-TABLEPROG.
           MOVE T2635-T2635-REC        TO ITEM-GENAREA.
           IF ITEM-STATUZ = MRNF
              MOVE WRITR               TO ITEM-FUNCTION
              MOVE ITEMREC             TO ITEM-FORMAT
           ELSE
              MOVE REWRT               TO ITEM-FUNCTION
           END-IF.

           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
           IF ITEM-ITEMSEQ           NOT = SPACES
              PERFORM 3200-CHECK-BLANK-LAST-RECORD
           END-IF.
      *
       3080-OTHER.
      *
       3090-EXIT.
            EXIT.
      *
       3100-CHECK-CHANGES SECTION.
      *--------------------------*
       3100-CHECK.
      *
           IF S2635-FLDIDS                  NOT =
              T2635-FLDIDS
               MOVE S2635-FLDIDS
                 TO T2635-FLDIDS
               MOVE 'Y' TO WSAA-UPDATE-FLAG.

      *
       3190-EXIT.
            EXIT.
      *
       3200-CHECK-BLANK-LAST-RECORD SECTION.
      *------------------------------------*
       3200-CHECK.
      *
           MOVE SPACES                 TO T2635-T2635-REC.
      *

      *
           IF ITEM-GENAREA NOT = T2635-T2635-REC
              GO TO 3290-EXIT
           END-IF.
      *
           MOVE ITEM-ITEMSEQ             TO WSAA-STORE-SEQNO.
           MOVE 99                     TO ITEM-ITEMSEQ.
      *
           MOVE ENDR                   TO ITEM-FUNCTION.
           CALL 'ITEMIO' USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
              MOVE ITEM-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
           IF ITEM-ITEMSEQ = WSAA-STORE-SEQNO
              MOVE DELTD               TO ITEM-FUNCTION
              CALL 'ITEMIO' USING ITEM-PARAMS
              IF ITEM-STATUZ              NOT = O-K
                 MOVE ITEM-PARAMS         TO SYSR-PARAMS
                 PERFORM 600-FATAL-ERROR
              END-IF
           END-IF.
      *
       3290-EXIT.
            EXIT.
      *
      /
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4020-PREPARATION.
      *
           IF SCRN-STATUZ              NOT = ROLU
                                   AND NOT = ROLD
              GO TO 4020-CONTINUE
           END-IF.
      *
      *
           IF SCRN-STATUZ              = ROLU
              ADD 1                    TO WSAA-SEQ
           ELSE
              SUBTRACT 1               FROM WSAA-SEQ
           END-IF.
      *
           GO TO 4080-OTHER.
      *
       4020-CONTINUE.
           ADD 1                       TO WSSP-PROGRAM-PTR.
           MOVE 'Y'                    TO WSAA-FIRST-TIME.
           MOVE ZEROS                  TO WSAA-SEQ.
      *
       4080-OTHER.
      *
       4090-EXIT.
            EXIT.
      *
      /
      *****************************************************************
      *     DUMMY CALL TO GENERATED PRINT PROGRAM TO ENSURE THAT
      *      IT IS TRANSFERED, TA/TR, ALONG WITH REST OF SUITE.
      *****************************************************************
      *
       5000-CALL-PRINT-PROGRAM SECTION.
      *********************************
      *
       5010-START.
      *
           CALL 'T2635PT'      USING   WSAA-TABLISTREC.
      *
       5090-EXIT.
            EXIT.
      *
