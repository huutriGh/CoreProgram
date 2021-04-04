      * Generation Parameters SCRVER(02)               Do Not Delete!   <S9503>
       IDENTIFICATION DIVISION.
       PROGRAM-ID.                                         P2645.
      *REMARKS.
      * REPLACE BY TABLE DESCRIPTION.
      *
      *
      *****************************************************************
      *              AMENDMENT  HISTORY                               *
      *****************************************************************
      * DATE.....   BY..   AMENDMENT...............  NUMBER
      *
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      * 30/09/98    DUNC  SMART 9503 Conv for Client/Server.        <S9503>
      * DD/MM/YY    X.X.   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  NNN
      *
      ***********************************************************************
      *                                                                     *
      * ......... New Version of the Amendment History.                     *
      *                                                                     *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      * 15/11/01  01/01   PCPPRT       Saw Geok Tin                         *
      *           Re-Compiled.                                         ??   *
      *           LETTYPE changed to window to T2634 instead of T3609.
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
       01  WSAA-PROG                   PIC X(05) VALUE 'P2645'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
       01  WSAA-UPDATE-FLAG            PIC X(01) VALUE 'N'.
       01  WSAA-TABLISTREC             PIC X(575).
      *
       01  ERRORS.
           03  E186                    PIC X(04) VALUE 'E186'.
      *
       01  TABLES.
           03  T2645                   PIC X(05) VALUE 'T2645'.
      *
       01  FORMATS.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
      /
           COPY VARCOM.
      *
           COPY SYSERRREC.
      *
      ***  COPY SCRNPARAMS.                                             <S9503>
      *
           COPY OPSTATSREC.
      /
      ***  COPY S2645SCR.                                               <S9503>
      /
           COPY T2645REC.
      /
           COPY ITEMSKM.
      /
           COPY DESCSKM.
      /
       LINKAGE SECTION.
      * Screen copybooks are now part of the linkage.                   <S9503>
      /                                                                 <S9503>
           COPY SCRNPARAMS.                                             <S9503>
      /                                                                 <S9503>
           COPY S2645SCR.                                               <S9503>
      *
           COPY WSSPCOMN.
      *
           COPY WSSPSMART.
      /
      * Statement now includes screen copybooks.                        <S9503>
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <S9503>
                                               SCRN-SCREEN-PARAMS       <S9503>
                                               S2645-DATA-AREA      .   <S9503>
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
       1010-INITIALISE.
      *
       1015-INITIALISE-SCREEN.
           MOVE SPACES                 TO S2645-DATA-AREA.
      *
       1020-READ-PRIMARY-RECORD.
      *
       1025-READ-RECORD.
           MOVE WSSP-ITEMKEY           TO ITEM-DATA-KEY.
           MOVE READR                  TO ITEM-FUNCTION.
      *
           CALL 'ITEMIO' USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
       1030-READ-SECONDARY-RECORDS.
      *
      *
       1031-READ-RECORD.
           MOVE ITEM-ITEMPFX           TO DESC-DESCPFX.
           MOVE ITEM-ITEMCOY           TO DESC-DESCCOY.
           MOVE ITEM-ITEMTABL          TO DESC-DESCTABL.
           MOVE ITEM-ITEMITEM          TO DESC-DESCITEM.
           MOVE SPACES                 TO DESC-ITEMSEQ
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
      *
           CALL 'DESCIO' USING DESC-PARAMS.
           IF DESC-STATUZ              NOT = O-K
                                   AND NOT = MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
      *
       1040-MOVE-TO-SCREEN.
           MOVE ITEM-ITEMCOY           TO S2645-COMPANY.
           MOVE ITEM-ITEMTABL          TO S2645-TABL.
           MOVE ITEM-ITEMITEM          TO S2645-ITEM.
      *
           MOVE DESC-LONGDESC          TO S2645-LONGDESC.
      *
           IF DESC-STATUZ              = MRNF
               MOVE SPACES             TO S2645-LONGDESC.
      *
           MOVE ITEM-GENAREA           TO T2645-T2645-REC.
      *
      *    IF THE EXTRA DATA AREA WAS NOT USED BEFORE THE NUMERIC
      *    FIELDS MUST BE SET TO ZERO TO AVOID DATA EXCEPTIONS.
      *
      *
           IF ITEM-GENAREA             NOT = SPACES
               GO TO 1045-GENERAL-AREA.
      *

      *
       1045-GENERAL-AREA.
           MOVE T2645-LETTER-TYPES
             TO S2645-LETTER-TYPES            .

      *
       1050-CONFIRMATION-FIELDS.
      *
       1080-OTHER.
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
      *                                                                 <S9503>
           IF WSSP-FLAG                = 'I'                            <S9503>
               MOVE PROT               TO SCRN-FUNCTION.                <S9503>
      *                                                                 <S9503>
           GO TO PRE-EXIT.                                              <S9503>
      *                                                                 <S9503>
       PRE-EXIT.                                                        <S9503>
           EXIT.                                                        <S9503>
      /                                                                 <S9503>
       2000-SCREEN-EDIT SECTION.
      **************************
      *
       2010-SCREEN-IO.
      *    CALL 'S2645IO' USING SCRN-SCREEN-PARAMS                      <S9503>
      *                          S2645-DATA-AREA.                       <S9503>
      * Screen errors are now handled in the calling program.           <S9503>
      *    PERFORM 200-SCREEN-ERRORS.                                   <S9503>
           MOVE O-K                    TO WSSP-EDTERROR.
      *
       2020-VALIDATE.
      *
           IF WSSP-FLAG                = 'I'
                GO TO 2090-EXIT.
      *
      *
      *    Enter screen validation here.
      *
      *
      *
       2080-OTHER.
      *
       2090-EXIT.
           IF S2645-ERROR-INDICATORS NOT = SPACES
               MOVE 'Y' TO WSSP-EDTERROR.
      *
       2095-EXIT.
            EXIT.
      /
      *****************************************************************
      *     UPDATE DATABASE IF REQUIRED AND LOG TRANSACTION
      *****************************************************************
      *
       3000-UPDATE SECTION.
      **********************
      *
       3010-PREPARATION.
      *
           IF WSSP-FLAG                = 'I'
               GO TO 3090-EXIT.
      *
       3030-CHECK-CHANGES.
      *
           MOVE 'N'                    TO WSAA-UPDATE-FLAG.
      *
           IF  WSSP-FLAG               = 'C'
               MOVE 'Y'                TO WSAA-UPDATE-FLAG
           END-IF.
           PERFORM 3100-CHECK-CHANGES.
      *
           IF WSAA-UPDATE-FLAG         NOT = 'Y'
               GO TO 3080-OTHER.
      *
       3050-UPDATE-PRIMARY-RECORD.
           MOVE READH                  TO ITEM-FUNCTION.
           MOVE WSSP-ITEMKEY           TO ITEM-DATA-KEY.
      *
           CALL 'ITEMIO' USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
      *
           MOVE WSSP-TRANID            TO VRCM-TRANID.
           MOVE VRCM-TERMID            TO VRCM-COMP-TERMID.
           MOVE VRCM-TRANID-N          TO VRCM-COMP-TRANID-N.
           MOVE VRCM-COMP-TRANID       TO ITEM-TRANID.
      *
       3055-UPDATE-RECORD.
      *
           MOVE WSAA-PROG              TO ITEM-TABLEPROG.
           MOVE T2645-T2645-REC        TO ITEM-GENAREA.
           MOVE REWRT                  TO ITEM-FUNCTION.
      *
           CALL 'ITEMIO' USING ITEM-PARAMS.
           IF ITEM-STATUZ              NOT = O-K
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
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
           IF S2645-LETTER-TYPES            NOT =
              T2645-LETTER-TYPES
               MOVE S2645-LETTER-TYPES
                 TO T2645-LETTER-TYPES
               MOVE 'Y' TO WSAA-UPDATE-FLAG.

      *
       3190-EXIT.
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
       4100-NEXT-PROGRAM.
           ADD 1                       TO WSSP-PROGRAM-PTR.
      *
       4900-EXIT.
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
           CALL 'T2645PT'      USING   WSAA-TABLISTREC.
      *
       5090-EXIT.
            EXIT.
      *
