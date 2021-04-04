      * Generation Parameters - SCRVER(02)            Do Not Delete|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PV029.
      *
      *(C) Copyright CSC Corporation Limited 1986 - 2000.
      *    All rights reserved. CSC Confidential.
      *
      *REMARKS.
      *
      * The remarks section should detail the main processes of the
      * program, and any special functions.
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 15/01/14  01/01   PHLRMS       Phuong Le Dev                        *
      *           Update Receipt Status                                     *
      *                                                                     *
      * 15/02/17  01/01   CS001        Tuyet Huynh IT - DEV                 *
      *           Update cancel receipt                                     *
      *                                                                     *
      * 13/09/17  01/01   CS002        Tuyet Huynh IT - DEV                 *
      *           Re-Assign Receipts.                                       *
      *                                                                     *
      * 03/01/18  01/01   CS006        Thanh Do                             *
      *           PRINT & POPUP PDF when update receipts as requested       *
      *                                                                     *
      * 10/04/19  01/01   PS041        Tuyet Huynh IT - DEV                 *
      *           Add status UR: Un-Returned.                               *
      *                                                                     *
      * 15/05/19  01/01   PS041        Vo Thi Hong Duyen - IT               *
      *           Update statcode RT when modify status UR.                 *
      *                                                                     *
      **DD/MM/YY*************************************************************
      /
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
       01  WSAA-PROG                   PIC X(05) VALUE 'PV029'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
       01  ERRORS.
           03  E005                    PIC X(04) VALUE 'E005'.
           03  H366                    PIC X(04) VALUE 'H366'.
           03  EV24                    PIC X(04) VALUE 'EV24'.
           03  RLCF                    PIC X(04) VALUE 'RLCF'.          <PHLRMS>
           03  E186                    PIC X(04) VALUE 'E186'.          <PHLRMS>
           03  E492                    PIC X(04) VALUE 'E492'.          <PHLRMS>
           03  EV57                    PIC X(04) VALUE 'EV57'.          <PHLRMS>
           03  G437                    PIC X(04) VALUE 'G437'.          <PHLRMS>
      *
       01  TABLES.
           03  TV020                   PIC X(05) VALUE 'TV020'.
           03  TV022                   PIC X(05) VALUE 'TV022'.
           03  TV036                   PIC X(05) VALUE 'TV036'.
           03  TV037                   PIC X(05) VALUE 'TV037'.
           03  TR384                   PIC X(05) VALUE 'TR384'.         <PHLRMS>
           03  TV047                   PIC X(05) VALUE 'TV047'.         <PHLRMS>
           03  TV058                   PIC X(05) VALUE 'TV058'.         <PHLRMS>
           03  TV098                   PIC X(05) VALUE 'TV098'.         <CS006>
      *
       01  FORMATS.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  TRRNREC                 PIC X(10) VALUE 'TRRNREC'.
           03  RETRREC                 PIC X(10) VALUE 'RETRREC'.       <PHLRMS>
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.       <PHLRMS>
           03  TRRNINFREC              PIC X(10) VALUE 'TRRNINFREC'.
           03  TRRNTRNREC              PIC X(10) VALUE 'TRRNTRNREC'.
           03  RETRTRNREC              PIC X(10) VALUE 'RETRTRNREC'.    <PHLRMS>
           03  RTRNDOCREC              PIC X(10) VALUE 'RTRNDOCREC'.
           03  CHDRLNBREC              PIC X(10) VALUE 'CHDRLNBREC'.
           03  ZPPRUSEREC              PIC X(10) VALUE 'ZPPRUSEREC'.
           03  RCPYACTREC              PIC X(10) VALUE 'RCPYACTREC'.
           03  AGNTLAGREC              PIC X(10) VALUE 'AGNTLAGREC'.    <PHLRMS>
           03  TRRNLOCREC              PIC X(10) VALUE 'TRRNLOCREC'.    <PHLRMS>
           03  TRRNMODREC              PIC X(10) VALUE 'TRRNMODREC'.    <PHLRMS>
           03  ZPPRCHKREC              PIC X(10) VALUE 'ZPPRCHKREC'.    <CS001>
           03  TRRNRASREC              PIC X(10) VALUE 'TRRNRASREC'.    <CS002>
           03  TRRNURTREC              PIC X(10) VALUE 'TRRNURTREC'.    <PS041>

       01  WSAA-BATCKEY.
           COPY BATCKEY.
      *
       01  WSAA-COLLECTOR              PIC X(10)                        <PHLRMS>
           VALUE 'Collector '.                                          <PHLRMS>
       01  WSAA-AGENT                  PIC X(10)                        <PHLRMS>
           VALUE 'Agent     '.                                          <PHLRMS>
       01  WSAA-OFFICE                 PIC X(10)                        <PHLRMS>
           VALUE 'Office    '.                                          <PHLRMS>
                                                                        <PHLRMS>
       01  WSAA-PAYEE-GIVN             PIC X(05) VALUE 'PYNMN'.         <PHLRMS>
       01  WSAA-AGENTNO                PIC X(08).                       <PHLRMS>
       01  WSAA-FLAG-PRINT             PIC X(01).                       <PHLRMS>
       01  WSAA-COUNT-PRINT            PIC 9(08).                       <PHLRMS>
       01  WSAA-FRMPFX                 PIC X(02).                       <PHLRMS>
       01  WSAA-FROMPER                PIC X(08).                       <PHLRMS>
       01  WSAA-TOPFX                  PIC X(02).                       <PHLRMS>
       01  WSAA-TOLOC                  PIC X(08).                       <PHLRMS>
       01  WSAA-FROMLOC                PIC X(08).                       <CS001>
       01  WSAA-TOPER                  PIC X(08).                       <CS001>
                                                                        <PHLRMS>
       01  WSAA-CURSOR-STAT            PIC 9(02).                       <PHLRMS>
       01  WSAA-CHECK-STATUS           PIC X(02).                       <PHLRMS>
       01  WSAA-COUNT                  PIC 9(08).                       <PHLRMS>
       01  WSAA-FLTSTA                 PIC X(02).                       <PHLRMS>
       01  IDX                         PIC 9(02).
       01  WSAA-SEQNUM                 PIC S9(07) COMP-3.               <PHLRMS>
       01  WSAA-VALID                  PIC X(01).
       01  WSAA-TABLE                  PIC X(05).
       01  WSAA-ITEM                   PIC X(08).
       01  WSAA-TODAY                  PIC S9(08).
       01  WSAA-RCTYPE                 PIC X(02).
       01  WSAA-RCSERFRM               PIC S9(08).
       01  WSAA-RCSERTO                PIC S9(08).
       01  WSAA-LP                     PIC X(02) VALUE 'LP'.
       01  WSAA-S                      PIC X(02) VALUE 'S '.
       01  WSAA-CRT-DATE               PIC S9(08).
       01  WSAA-POL-NUM                PIC X(08).
       01  WSAA-AGT-NUM                PIC X(08).
       01  WSAA-FOUND                  PIC X(01).
       01  WSAA-RCSERNUM               PIC S9(08).
       01  WSAA-TRANNO                 PIC S9(05).
       01  WSAA-OFFCDE                 PIC X(05).
       01  WSAA-SELECT                 PIC X(01).
       01  WSAA-CHDRNUM                PIC X(08).
       01  WSAA-LOADED                 PIC X(01).                       <PHLRMS>
TDO   * CS006 Print and Popup PDF file when printing report             <CS006>
       01  WSAA-COMPANY                PIC X(01) VALUE '2'.             <CS006>
       01  WSAA-FUNCTION               PIC X(05).                       <CS006>
       01  WSAA-MODE                   PIC X(01).                       <CS006>
       01  WSAA-LANGUAGE               PIC X(01).                       <CS006>
       01  WSAA-EMAIL                  PIC X(50) VALUE SPACES.          <CS006>
       01  WSAA-ESUBJECT               PIC X(50) VALUE SPACES.          <CS006>
       01  WSAA-ETEXT                  PIC X(450) VALUE SPACES.         <CS006>
       01  WSAA-POPUP-PDF              PIC X(01).                       <CS006>
       01  WSAA-LETC-RRN               PIC S9(09) COMP-3.               <CS006>
       01  WSAA-STATUZ                 PIC X(04).                       <CS006>
      *                                                                 <CS006>
      /
           COPY VARCOM.
           COPY SMTPFXCPY.
           COPY TV036REC.
           COPY TV037REC.
           COPY TV058REC.                                               <PHLRMS>
           COPY TR384REC.                                               <PHLRMS>
           COPY DATCON1REC.
      *
           COPY SYSERRREC.
      *
           COPY LETRQSTREC.                                             <PHLRMS>
           COPY TV020REC.                                               <PHLRMS>
           COPY TV047REC.                                               <PHLRMS>
           COPY DESCSKM.
           COPY ITEMSKM.
           COPY OPSTATSREC.
           COPY TRRNSKM.
           COPY RETRSKM.                                                <PHLRMS>
           COPY CLTSSKM.                                                <PHLRMS>
           COPY RETRTRNSKM.                                             <PHLRMS>
           COPY TRRNINFSKM.
           COPY TRRNTRNSKM.
           COPY RTRNDOCSKM.
           COPY CHDRLNBSKM.
           COPY ZPPRUSESKM.
           COPY RCPYACTSKM.
           COPY AGNTLAGSKM.                                             <PHLRMS>
           COPY TRRNLOCSKM.                                             <PHLRMS>
           COPY TRRNMODSKM.                                             <PHLRMS>
           COPY ZPPRCHKSKM.                                             <CS001>
           COPY NAMADRSREC.                                             <PHLRMS>
           COPY TRRNRASSKM.                                             <CS002>
           COPY TRRNURTSKM.                                             <PS041>
TDO        COPY TV098REC.                                               <CS006>
      /
       LINKAGE SECTION.

           COPY WSSPCOMN.

       01  WSSP-USER-AREA              PIC X(768).

           COPY SCRNPARAMS.

           COPY SV029SCR.
      /
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA
                                SCRN-SCREEN-PARAMS SV029-DATA-AREA
                                SV029-SUBFILE-AREA.

           COPY MAING.
           COPY CONFNAME.                                               <PHLRMS>
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
           INITIALIZE                     SV029-DATA-AREA.
           INITIALIZE                     SV029-SUBFILE-AREA.
           MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
           MOVE WSAA-PROG              TO SV029-PROGNAM.                <PHLRMS>

******     MOVE VRCM-MAX-DATE                                           <PHLRMS>
******          TO SV029-EXPDTE                   .
                                                                        <PHLRMS>
           MOVE SPACES                 TO WSAA-FLTSTA.                  <PHLRMS>

      *
      *-- Retrieve key from SV024
      *
           MOVE WSSP-UND-FUNCTION      TO WSAA-RCTYPE.
           MOVE WSSP-CURRFROM          TO WSAA-RCSERFRM.
           MOVE WSSP-CURRTO            TO WSAA-RCSERTO.
      *
      *-- Get Business Date
      *
           MOVE TDAY                   TO DTC1-FUNCTION.

           CALL 'DATCON1'              USING DTC1-DATCON1-REC.

           IF DTC1-STATUZ           NOT = O-K
               MOVE DTC1-STATUZ        TO SYSR-STATUZ
               MOVE DTC1-DATCON1-REC   TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE DTC1-INT-DATE          TO WSAA-TODAY.
      *
      *-- Office Code
      *
           MOVE WSSP-CHDR-CHDRNUM      TO WSAA-OFFCDE.                  <PHLRMS>

      * Dummy subfile initalisation for prototype - relpace with SCLR
           MOVE SCLR                   TO SCRN-FUNCTION.

           CALL 'SV029IO' USING SCRN-SCREEN-PARAMS
                                SV029-DATA-AREA
                                SV029-SUBFILE-AREA.

           IF SCRN-STATUZ NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

      *    Dummy field initilisation for prototype version.
      *
      *    Set screen fields
      *
           MOVE WSSP-SERVUNIT          TO SV029-DENTTYP.                <PHLRMS>
           MOVE WSSP-COMMON-AREA(102:8)                                 <PHLRMS>
                                       TO SV029-ENTYNUM.                <PHLRMS>
                                                                        <PHLRMS>
           IF  SV029-DENTTYP           = 'AG'                           <PHLRMS>
               MOVE WSAA-AGENT         TO SV029-ENT-01                  <PHLRMS>
               MOVE WSAA-AGENT         TO SV029-ENT-02                  <PHLRMS>
               INITIALIZE                 AGNTLAG-PARAMS                <PHLRMS>
               MOVE WSSP-COMPANY       TO AGNTLAG-AGNTCOY               <PHLRMS>
               MOVE SV029-ENTYNUM      TO AGNTLAG-AGNTNUM               <PHLRMS>
               MOVE AGNTLAGREC         TO AGNTLAG-FORMAT                <PHLRMS>
               MOVE READR              TO AGNTLAG-FUNCTION              <PHLRMS>
                                                                        <PHLRMS>
               CALL 'AGNTLAGIO'        USING AGNTLAG-PARAMS             <PHLRMS>
                                                                        <PHLRMS>
               IF  AGNTLAG-STATUZ      NOT = O-K                        <PHLRMS>
                   MOVE AGNTLAG-STATUZ TO SYSR-STATUZ                   <PHLRMS>
                   MOVE AGNTLAG-PARAMS TO SYSR-PARAMS                   <PHLRMS>
                   PERFORM 600-FATAL-ERROR                              <PHLRMS>
               END-IF                                                   <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Get Agent Name                                               <PHLRMS>
      *                                                                 <PHLRMS>
               PERFORM 1100-GET-AGNT-INFO                               <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  SV029-DENTTYP           = 'CL'                           <PHLRMS>
               MOVE WSAA-COLLECTOR     TO SV029-ENT-01                  <PHLRMS>
               MOVE WSAA-COLLECTOR     TO SV029-ENT-02                  <PHLRMS>
               MOVE TV047              TO WSAA-TABLE                    <PHLRMS>
               MOVE SV029-ENTYNUM      TO WSAA-ITEM                     <PHLRMS>
               PERFORM 1900-READ-TABLE                                  <PHLRMS>
               MOVE ITEM-GENAREA       TO TV047-TV047-REC               <PHLRMS>
               MOVE TV047-MEMNAME      TO SV029-CLTNAME                 <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  SV029-DENTTYP           = 'OF'                           <PHLRMS>
               MOVE WSAA-OFFICE        TO SV029-ENT-01                  <PHLRMS>
               MOVE WSAA-OFFICE        TO SV029-ENT-02                  <PHLRMS>
               MOVE TV022              TO WSAA-TABLE                    <PHLRMS>
               MOVE SV029-ENTYNUM      TO WSAA-ITEM                     <PHLRMS>
               PERFORM 1800-READ-DESC                                   <PHLRMS>
               MOVE DESC-LONGDESC      TO SV029-CLTNAME                 <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *
           PERFORM 1700-READ-TV037.
      *
      ***  PERFORM  1200-LOAD-SUBFILE.                                  <PHLRMS>
                                                                        <PHLRMS>
           EVALUATE SV029-DENTTYP                                       <PHLRMS>
           WHEN 'OF'                                                    <PHLRMS>
               PERFORM 2300-LOAD-OFF-RCPT                               <PHLRMS>
           WHEN OTHER                                                   <PHLRMS>
               PERFORM 2100-LOAD-AGCL-RCPT                              <PHLRMS>
               PERFORM A2100-LOAD-RAS-RCPT                              <CS002>
               PERFORM B2100-LOAD-UR-RCPT                               <PS041>
           END-EVALUATE.                                                <PHLRMS>
           MOVE 'Y'                    TO WSAA-LOADED.                  <PHLRMS>
           MOVE 1                      TO SCRN-SUBFILE-RRN.             <PHLRMS>
      *
       1090-EXIT.
            EXIT.
      /                                                                 <PHLRMS>
       1100-GET-AGNT-INFO SECTION.                                      <PHLRMS>
      ****************************                                      <PHLRMS>
      *                                                                 <PHLRMS>
       1110-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE SPACES                 TO NMAD-NAMADRS-REC.             <PHLRMS>
           MOVE AGNTLAG-CLNTPFX        TO NMAD-CLNT-PREFIX              <PHLRMS>
           MOVE AGNTLAG-CLNTCOY        TO NMAD-CLNT-COMPANY             <PHLRMS>
           MOVE AGNTLAG-CLNTNUM        TO NMAD-CLNT-NUMBER              <PHLRMS>
           MOVE WSSP-LANGUAGE          TO NMAD-LANGUAGE.                <PHLRMS>
           MOVE WSAA-PAYEE-GIVN        TO NMAD-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'NAMADRS'              USING NMAD-NAMADRS-REC.          <PHLRMS>
                                                                        <PHLRMS>
           IF  NMAD-STATUZ             NOT = O-K                        <PHLRMS>
               MOVE NMAD-NAMADRS-REC   TO SYSR-PARAMS                   <PHLRMS>
               MOVE NMAD-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF NMAD-STATUZ              = MRNF                           <PHLRMS>
              MOVE ALL '?'             TO SV029-CLTNAME                 <PHLRMS>
           ELSE                                                         <PHLRMS>
              MOVE NMAD-NAME           TO SV029-CLTNAME                 <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       1190-EXIT.                                                       <PHLRMS>
            EXIT.                                                       <PHLRMS>
      /
       1200-LOAD-SUBFILE SECTION.
      ***************************
      *                                                                 <PHLRMS>
       1210-INIT.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     SV029-SUBFILE-AREA.           <PHLRMS>
                                                                        <PHLRMS>
           MOVE SCLR                   TO SCRN-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'SV029IO'              USING SCRN-SCREEN-PARAMS         <PHLRMS>
                                             SV029-DATA-AREA            <PHLRMS>
                                             SV029-SUBFILE-AREA.        <PHLRMS>
                                                                        <PHLRMS>
           IF  SCRN-STATUZ NOT         = O-K                            <PHLRMS>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE ZEROES                 TO WSAA-COUNT.                   <PHLRMS>
           INITIALIZE                     TRRNINF-PARAMS.               <PHLRMS>
           MOVE WSAA-RCTYPE            TO TRRNINF-RCTYPE.               <PHLRMS>
           MOVE WSAA-RCSERFRM          TO TRRNINF-RCSERNUM.             <PHLRMS>
           MOVE TRRNINFREC             TO TRRNINF-FORMAT.               <PHLRMS>
           MOVE BEGN                   TO TRRNINF-FUNCTION.             <PHLRMS>
      *
       1220-START.                                                      <PHLRMS>
      *
           CALL 'TRRNINFIO'            USING TRRNINF-PARAMS.

           IF  TRRNINF-STATUZ          NOT = O-K                        <PHLRMS>
           AND                         NOT = ENDP
               MOVE TRRNINF-STATUZ     TO SYSR-STATUZ
               MOVE TRRNINF-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  TRRNINF-STATUZ          NOT = O-K                        <PHLRMS>
           OR  TRRNINF-RCTYPE          NOT = WSAA-RCTYPE                <PHLRMS>
           OR  TRRNINF-RCSERNUM        > WSAA-RCSERTO                   <PHLRMS>
               IF  WSAA-COUNT          = ZEROES                         <PHLRMS>
                   PERFORM 2200-ADD-NULL-SUBFILE                        <PHLRMS>
               END-IF                                                   <PHLRMS>
               GO TO 1290-EXIT
           END-IF.
      *
      *--  Filter status                                                <PHLRMS>
      *
           MOVE TRRNINF-RCSTAT         TO WSAA-CHECK-STATUS.            <PHLRMS>
           PERFORM 1600-CHECK-STATUS.                                   <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-VALID              = 'N'                            <PHLRMS>
               GO TO 1280-NEXT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>

           IF  SV029-FLTSTA            NOT = SPACES                     <PHLRMS>
           AND TRRNINF-RCSTAT          NOT = SV029-FLTSTA               <PHLRMS>
               GO TO 1280-NEXT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           ADD 1                       TO WSAA-COUNT.                   <PHLRMS>
           MOVE 'Y'                    TO SV029-PROCFLG.                <PHLRMS>
           MOVE 'Y'                    TO SV029-STATCDE-OUT(PR).        <PHLRMS>
           MOVE 'N'                    TO SV029-STATCDE-OUT(ND).        <PHLRMS>
           MOVE 'N'                    TO SV029-NEWSTS-OUT (ND).        <PHLRMS>
           MOVE 'N'                    TO SV029-NEWSTS-OUT (PR).        <PHLRMS>
           MOVE TRRNINF-RCNUM          TO SV029-RCNUM.
           MOVE TRRNINF-RCSTAT         TO SV029-STATCDE.

           MOVE TV020                  TO WSAA-TABLE.
           MOVE TRRNINF-RCSTAT         TO WSAA-ITEM.
           PERFORM 1800-READ-DESC.
           MOVE DESC-LONGDESC          TO SV029-STAT.

           MOVE TRRNINF-EXPDTE         TO SV029-EXPDTE.
           MOVE SPACES                 TO SV029-NEWSTS                  <PHLRMS>
                                          SV029-RSNUPD.                 <PHLRMS>
           MOVE TRRNINF-TOPER          TO SV029-AGENTNO.
      *
      *--  If receipt is assigned, get Policy Number
      *
           PERFORM 3500-READ-RCPY.
           MOVE WSAA-CHDRNUM           TO SV029-POLNO.

           PERFORM 1300-READ-ZPPR.                                      <PHLRMS>
      *
       1270-SADD.                                                       <PHLRMS>
      *
           MOVE SADD                   TO SCRN-FUNCTION.

           CALL 'SV029IO'              USING SCRN-SCREEN-PARAMS         <PHLRMS>
                                             SV029-DATA-AREA            <PHLRMS>
                                             SV029-SUBFILE-AREA.        <PHLRMS>

           IF  SCRN-STATUZ             NOT = O-K AND ENDP               <PHLRMS>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.
      *
       1280-NEXT.
      *
           MOVE  NEXTR                 TO TRRNINF-FUNCTION.
           GO TO 1220-START.                                            <PHLRMS>
      *
       1290-EXIT.
           EXIT.
      /
       1300-READ-ZPPR SECTION.                                          <PHLRMS>
      ************************                                          <PHLRMS>
      *
       1310-START.
      *
           INITIALIZE                     ZPPRUSE-PARAMS.
           MOVE 'CA'                   TO ZPPRUSE-PREFIX.
           MOVE WSAA-RCTYPE            TO ZPPRUSE-RCPREPRTCD.
           MOVE SV029-RCNUM(3:8)       TO ZPPRUSE-RCPREPRNT.
           MOVE ZPPRUSEREC             TO ZPPRUSE-FORMAT.
           MOVE READR                  TO ZPPRUSE-FUNCTION.

           CALL 'ZPPRUSEIO'            USING ZPPRUSE-PARAMS.

           IF  ZPPRUSE-STATUZ          NOT = O-K
           AND                         NOT = MRNF
               MOVE ZPPRUSE-STATUZ     TO SYSR-STATUZ
               MOVE ZPPRUSE-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  ZPPRUSE-STATUZ          = O-K
               PERFORM 1400-READ-RTRNDOC
           END-IF.
      *
       1390-EXIT.
           EXIT.
      /
       1400-READ-RTRNDOC SECTION.
      ***************************
      *
       1410-START.
      *
           MOVE SPACES                 TO WSAA-POL-NUM.
           MOVE SPACES                 TO WSAA-AGT-NUM.

           INITIALIZE                     RTRNDOC-PARAMS.
           MOVE ZPPRUSE-RECEIPT        TO RTRNDOC-RDOCNUM.
           MOVE WSAA-LP                TO RTRNDOC-SACSCODE.
           MOVE WSAA-S                 TO RTRNDOC-SACSTYP.
           MOVE RTRNDOCREC             TO RTRNDOC-FORMAT.
           MOVE READR                  TO RTRNDOC-FUNCTION.

           CALL 'RTRNDOCIO'            USING RTRNDOC-PARAMS.

           IF  RTRNDOC-STATUZ          NOT = O-K
           AND                         NOT = MRNF
               MOVE RTRNDOC-STATUZ     TO SYSR-STATUZ
               MOVE RTRNDOC-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  RTRNDOC-STATUZ          NOT = O-K
               GO TO 1490-EXIT
           END-IF.

           MOVE RTRNDOC-RLDGACCT       TO WSAA-POL-NUM.

           PERFORM 1500-READ-CHDR.
      *
       1490-EXIT.
           EXIT.
      /
       1500-READ-CHDR SECTION.
      ************************
      *
       1510-START.
      *
           INITIALIZE                     CHDRLNB-PARAMS.
           MOVE WSSP-COMPANY           TO CHDRLNB-CHDRCOY.
           MOVE WSAA-POL-NUM           TO CHDRLNB-CHDRNUM.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
           MOVE READR                  TO CHDRLNB-FUNCTION.

           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.

           IF  CHDRLNB-STATUZ          NOT = O-K
           AND                         NOT = MRNF
               MOVE CHDRLNB-STATUZ     TO SYSR-STATUZ
               MOVE CHDRLNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  CHDRLNB-STATUZ          NOT = O-K
               GO TO 1590-EXIT
           END-IF.

           MOVE CHDRLNB-AGNTNUM        TO WSAA-AGT-NUM.
      *
       1590-EXIT.
           EXIT.
      /                                                                 <PHLRMS>
       1600-CHECK-STATUS SECTION.                                       <PHLRMS>
      ***************************                                       <PHLRMS>
      *                                                                 <PHLRMS>
       1610-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE 'Y'                    TO WSAA-VALID.                   <PHLRMS>
                                                                        <PHLRMS>
           MOVE 'N'                    TO WSAA-FOUND.                   <PHLRMS>
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 20               <PHLRMS>
                                   OR WSAA-FOUND = 'Y'                  <PHLRMS>
                                   OR TV037-NCURSTAT (IDX) = SPACES     <PHLRMS>
               IF  TV037-NCURSTAT(IDX) = WSAA-CHECK-STATUS              <PHLRMS>
                   MOVE 'Y'            TO WSAA-FOUND                    <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-PERFORM.                                                 <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-FOUND              = 'Y'                            <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Not allow to update                                          <PHLRMS>
      *                                                                 <PHLRMS>
               MOVE 'N'                TO WSAA-VALID                    <PHLRMS>
           ELSE                                                         <PHLRMS>
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 20           <PHLRMS>
                                    OR WSAA-FOUND = 'Y'                 <PHLRMS>
                                    OR TV037-CURSTAT(IDX)  = SPACES     <PHLRMS>
                   IF  TV037-CURSTAT(IDX) = WSAA-CHECK-STATUS           <PHLRMS>
                       MOVE 'Y'        TO WSAA-FOUND                    <PHLRMS>
                   END-IF                                               <PHLRMS>
               END-PERFORM                                              <PHLRMS>
      *                                                                 <PHLRMS>
      *--  If not parameter, allow all statuses                         <PHLRMS>
      *                                                                 <PHLRMS>
               IF  WSAA-FOUND          = 'N'                            <PHLRMS>
               AND IDX                 NOT = 1                          <PHLRMS>
                   MOVE 'N'            TO WSAA-VALID                    <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       1690-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /
       1700-READ-TV037 SECTION.
      *************************
      *
       1710-START.
      *
           INITIALIZE                     TV037-TV037-REC.
           MOVE TV037                  TO WSAA-TABLE.
           MOVE WSKY-BATC-BATCTRCDE    TO WSAA-ITEM.
           PERFORM 1900-READ-TABLE.
           MOVE ITEM-GENAREA           TO TV037-TV037-REC.
      *
       1790-EXIT.
           EXIT.
      /
       1800-READ-DESC SECTION.
      ************************
      *
       1810-START.
      *
           MOVE SPACES                 TO DESC-DATA-KEY.
           MOVE SMTP-ITEM              TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE WSAA-TABLE             TO DESC-DESCTABL.
           MOVE WSAA-ITEM              TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
           MOVE DESCREC                TO DESC-FORMAT.

           CALL 'DESCIO'               USING DESC-PARAMS.

           IF DESC-STATUZ              NOT = O-K
              MOVE '???'               TO DESC-LONGDESC
           END-IF.
      *
       1890-EXIT.
           EXIT.
      /
       1900-READ-TABLE SECTION.
      *************************
      *
       1910-START.
      *
           INITIALIZE                    ITEM-PARAMS.
           MOVE SMTP-ITEM             TO ITEM-ITEMPFX.
           MOVE WSSP-COMPANY          TO ITEM-ITEMCOY.
           MOVE WSAA-TABLE            TO ITEM-ITEMTABL.                 <PHLRMS>
           MOVE WSAA-ITEM             TO ITEM-ITEMITEM.
           MOVE ITEMREC               TO ITEM-FORMAT.
           MOVE READR                 TO ITEM-FUNCTION.

           CALL  'ITEMIO'             USING ITEM-PARAMS.

           IF  ITEM-STATUZ            NOT = O-K
           AND ITEM-STATUZ            NOT = MRNF
               MOVE ITEM-PARAMS       TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       1990-EXIT.
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
           MOVE O-K                    TO WSSP-EDTERROR.

           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'
              MOVE 3000                TO WSSP-SECTIONNO
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
           IF  SCRN-STATUZ             = CALC
               MOVE 'Y'                TO WSSP-EDTERROR
           END-IF.
           IF  WSAA-FLTSTA             NOT = SV029-FLTSTA               <PHLRMS>
               MOVE SV029-FLTSTA       TO WSAA-FLTSTA                   <PHLRMS>
               MOVE 'N'                TO WSAA-LOADED                   <PHLRMS>
               MOVE 'Y'                TO WSSP-EDTERROR                 <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *
       2010-VALIDATE-SCREEN.
      *                                                                 <PHLRMS>
           IF  WSAA-LOADED             NOT = 'Y'                        <PHLRMS>
      ***      PERFORM 1200-LOAD-SUBFILE                                <PHLRMS>
               EVALUATE SV029-DENTTYP                                   <PHLRMS>
               WHEN 'OF'                                                <PHLRMS>
                   PERFORM 2300-LOAD-OFF-RCPT                           <PHLRMS>
               WHEN OTHER                                               <PHLRMS>
                   PERFORM 2100-LOAD-AGCL-RCPT                          <PHLRMS>
                   PERFORM A2100-LOAD-RAS-RCPT                          <CS002>
                   PERFORM B2100-LOAD-UR-RCPT                           <PS041>
               END-EVALUATE                                             <PHLRMS>
               MOVE 1                  TO SCRN-SUBFILE-RRN              <PHLRMS>
               MOVE 'Y'                TO WSAA-LOADED                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *
       2050-CHECK-FOR-ERRORS.
      *
           IF SV029-ERROR-INDICATORS   NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
              GO TO 2090-EXIT
           END-IF.
      *
       2060-VALIDATE-SUBFILE.
      *
           MOVE SSTRT                  TO SCRN-FUNCTION.

           CALL 'SV029IO' USING SCRN-SCREEN-PARAMS
                                SV029-DATA-AREA
                                SV029-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
           AND                         NOT = ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           PERFORM 2600-VALIDATE-SUBFILE
                                       UNTIL SCRN-STATUZ = ENDP.

           MOVE SPACES                 TO SV029-FILSTS.                 <PHLRMS>
           MOVE SPACES                 TO SV029-FLTRSN.                 <PHLRMS>

           MOVE 1                      TO SCRN-SUBFILE-RRN.
      *
       2090-EXIT.
            EXIT.
      /                                                                 <PHLRMS>
       2100-LOAD-AGCL-RCPT SECTION.                                     <PHLRMS>
      *****************************                                     <PHLRMS>
      *                                                                 <PHLRMS>
       2110-INIT.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     SV029-SUBFILE-AREA.           <PHLRMS>
                                                                        <PHLRMS>
           MOVE SCLR                   TO SCRN-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'SV029IO'              USING SCRN-SCREEN-PARAMS         <PHLRMS>
                                             SV029-DATA-AREA            <PHLRMS>
                                             SV029-SUBFILE-AREA.        <PHLRMS>
                                                                        <PHLRMS>
           IF  SCRN-STATUZ NOT         = O-K                            <PHLRMS>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE ZEROES                 TO WSAA-COUNT.                   <PHLRMS>
           INITIALIZE                     TRRNMOD-PARAMS.               <PHLRMS>
           MOVE SV029-ENTYNUM          TO TRRNMOD-TOPER.                <PHLRMS>
           MOVE WSAA-RCTYPE            TO TRRNMOD-RCTYPE.               <PHLRMS>
           MOVE WSAA-RCSERFRM          TO TRRNMOD-RCSERNUM.             <PHLRMS>
           MOVE TRRNMODREC             TO TRRNMOD-FORMAT.               <PHLRMS>
           MOVE BEGN                   TO TRRNMOD-FUNCTION.             <PHLRMS>
      *                                                                 <PHLRMS>
       2120-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           CALL 'TRRNMODIO'            USING TRRNMOD-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF TRRNMOD-STATUZ           NOT = O-K                        <PHLRMS>
           AND                         NOT = ENDP                       <PHLRMS>
               MOVE TRRNMOD-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE TRRNMOD-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNMOD-STATUZ          = ENDP                           <PHLRMS>
           OR  TRRNMOD-TOPER           NOT = SV029-ENTYNUM              <PHLRMS>
           OR  TRRNMOD-RCTYPE          NOT = WSAA-RCTYPE                <PHLRMS>
           OR  TRRNMOD-RCSERNUM        > WSAA-RCSERTO                   <PHLRMS>
      *        IF  WSAA-COUNT          = ZEROES                 <PS041> <PHLRMS>
      *            PERFORM 2200-ADD-NULL-SUBFILE                <PS041> <PHLRMS>
      *        END-IF                                           <PS041> <PHLRMS>
               GO TO 2190-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Filter status                                                <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE TRRNMOD-RCSTAT         TO WSAA-CHECK-STATUS.            <PHLRMS>
           PERFORM 1600-CHECK-STATUS.                                   <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-VALID              = 'N'                            <PHLRMS>
               GO TO 2180-NEXT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  SV029-FLTSTA            NOT = SPACES                     <PHLRMS>
           AND TRRNMOD-RCSTAT          NOT = SV029-FLTSTA               <PHLRMS>
               GO TO 2180-NEXT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Check Location                                               <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     TV020-TV020-REC.              <PHLRMS>
           MOVE TV020                  TO WSAA-TABLE     .              <PHLRMS>
           MOVE TRRNMOD-RCSTAT         TO WSAA-ITEM      .              <PHLRMS>
           PERFORM 1900-READ-TABLE.                                     <PHLRMS>
           MOVE ITEM-GENAREA           TO TV020-TV020-REC.              <PHLRMS>
                                                                        <PHLRMS>
           IF  TV020-KEYLOCN           = 'Y'                            <PHLRMS>
           AND WSAA-OFFCDE             NOT = TRRNMOD-LOCALITY           <PHLRMS>
               GO TO 2180-NEXT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           ADD 1                       TO WSAA-COUNT.                   <PHLRMS>
           MOVE 'Y'                    TO SV029-PROCFLG.                <PHLRMS>
           MOVE 'Y'                    TO SV029-STATCDE-OUT(PR).        <PHLRMS>
           MOVE 'N'                    TO SV029-STATCDE-OUT(ND).        <PHLRMS>
           MOVE 'N'                    TO SV029-DOCTDESC-OUT(ND).       <PHLRMS>
           MOVE 'N'                    TO SV029-DOCTDESC-OUT(PR).       <PHLRMS>
           MOVE 'N'                    TO SV029-NEWSTS-OUT(ND).         <PHLRMS>
           MOVE 'N'                    TO SV029-NEWSTS-OUT(PR).         <PHLRMS>
           MOVE TRRNMOD-RCNUM          TO SV029-RCNUM.                  <PHLRMS>
           MOVE TRRNMOD-RCSTAT         TO SV029-STATCDE.                <PHLRMS>
                                                                        <PHLRMS>
           MOVE TV020                  TO WSAA-TABLE.                   <PHLRMS>
           MOVE TRRNMOD-RCSTAT         TO WSAA-ITEM.                    <PHLRMS>
           PERFORM 1800-READ-DESC.                                      <PHLRMS>
           MOVE DESC-LONGDESC          TO SV029-STAT.                   <PHLRMS>
                                                                        <PHLRMS>
           MOVE TRRNMOD-EXPDTE         TO SV029-EXPDTE.                 <PHLRMS>
           MOVE SPACES                 TO SV029-NEWSTS                  <PHLRMS>
                                          SV029-RSNUPD.                 <PHLRMS>
      *                                                                 <PHLRMS>
      *--  If receipt is assigned, get Policy Number , Agent No         <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM 3500-READ-RCPY.                                      <PHLRMS>
           MOVE WSAA-AGENTNO           TO SV029-AGENTNO.                <PHLRMS>
           MOVE WSAA-CHDRNUM           TO SV029-POLNO.                  <PHLRMS>
      *                                                                 <PHLRMS>
       2170-SADD.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE SADD                   TO SCRN-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'SV029IO'              USING SCRN-SCREEN-PARAMS         <PHLRMS>
                                             SV029-DATA-AREA            <PHLRMS>
                                             SV029-SUBFILE-AREA.        <PHLRMS>
                                                                        <PHLRMS>
           IF  SCRN-STATUZ             NOT = O-K AND ENDP               <PHLRMS>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       2180-NEXT.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE  NEXTR                 TO TRRNMOD-FUNCTION.             <PHLRMS>
           GO TO 2120-START.                                            <PHLRMS>
      *                                                                 <PHLRMS>
       2190-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       A2100-LOAD-RAS-RCPT SECTION.                                     <CS002>
      *****************************                                     <CS002>
       A2101-START.                                                     <CS002>
      *                                                                 <CS002>
           INITIALIZE                     TRRNRAS-PARAMS.               <CS002>
           MOVE SV029-ENTYNUM          TO TRRNRAS-TOPER.                <CS002>
           MOVE WSAA-RCTYPE            TO TRRNRAS-RCTYPE.               <CS002>
           MOVE WSAA-RCSERFRM          TO TRRNRAS-RCSERNUM.             <CS002>
           MOVE SPACES                 TO TRRNRAS-LOCALITY.             <CS002>
           MOVE TRRNRASREC             TO TRRNRAS-FORMAT.               <CS002>
           MOVE BEGN                   TO TRRNRAS-FUNCTION.             <CS002>
      *                                                                 <CS002>
       A2102-LOOP.                                                      <CS002>
      *                                                                 <CS002>
           CALL 'TRRNRASIO'            USING TRRNRAS-PARAMS.            <CS002>
                                                                        <CS002>
           IF  TRRNRAS-STATUZ          NOT = O-K                        <CS002>
           AND                         NOT = ENDP                       <CS002>
               MOVE TRRNRAS-PARAMS     TO SYSR-PARAMS                   <CS002>
               PERFORM 600-FATAL-ERROR                                  <CS002>
           END-IF.                                                      <CS002>
                                                                        <CS002>
           IF  TRRNRAS-STATUZ          = ENDP                           <CS002>
           OR  TRRNRAS-TOPER           NOT = SV029-ENTYNUM              <CS002>
           OR  TRRNRAS-RCTYPE          NOT = WSAA-RCTYPE                <CS002>
           OR  TRRNRAS-RCSERNUM        > WSAA-RCSERTO                   <CS002>
      *        IF  WSAA-COUNT          = ZEROES                 <PS041> <CS002>
      *            PERFORM 2200-ADD-NULL-SUBFILE                <PS041> <CS002>
      *        END-IF                                           <PS041> <CS002>
               GO TO A2109-EXIT                                         <CS002>
           END-IF.                                                      <CS002>
      *                                                                 <CS002>
      *--  Filter status                                                <CS002>
      *                                                                 <CS002>
           MOVE TRRNRAS-RCSTAT         TO WSAA-CHECK-STATUS.            <CS002>
           PERFORM 1600-CHECK-STATUS.                                   <CS002>
                                                                        <CS002>
           IF  WSAA-VALID              = 'N'                            <CS002>
               GO TO A2108-NEXT                                         <CS002>
           END-IF.                                                      <CS002>
                                                                        <CS002>
           IF  SV029-FLTSTA            NOT = SPACES                     <CS002>
           AND TRRNRAS-RCSTAT          NOT = SV029-FLTSTA               <CS002>
               GO TO A2108-NEXT                                         <CS002>
           END-IF.                                                      <CS002>
      *                                                                 <CS002>
      *--  Check Location                                               <CS002>
      *                                                                 <CS002>
           INITIALIZE                     TV020-TV020-REC.              <CS002>
           MOVE TV020                  TO WSAA-TABLE     .              <CS002>
           MOVE TRRNRAS-RCSTAT         TO WSAA-ITEM      .              <CS002>
           PERFORM 1900-READ-TABLE.                                     <CS002>
           MOVE ITEM-GENAREA           TO TV020-TV020-REC.              <CS002>
                                                                        <CS002>
           IF  TV020-KEYLOCN           = 'Y'                            <CS002>
           AND WSAA-OFFCDE             NOT = TRRNRAS-LOCALITY           <CS002>
               GO TO A2108-NEXT                                         <CS002>
           END-IF.                                                      <CS002>
                                                                        <CS002>
           ADD 1                       TO WSAA-COUNT.                   <CS002>
           MOVE 'Y'                    TO SV029-PROCFLG.                <CS002>
           MOVE 'Y'                    TO SV029-STATCDE-OUT(PR).        <CS002>
           MOVE 'N'                    TO SV029-STATCDE-OUT(ND).        <CS002>
           MOVE 'N'                    TO SV029-DOCTDESC-OUT(ND).       <CS002>
           MOVE 'N'                    TO SV029-DOCTDESC-OUT(PR).       <CS002>
           MOVE 'N'                    TO SV029-NEWSTS-OUT(ND).         <CS002>
           MOVE 'N'                    TO SV029-NEWSTS-OUT(PR).         <CS002>
           MOVE TRRNRAS-RCNUM          TO SV029-RCNUM.                  <CS002>
           MOVE TRRNRAS-RCSTAT         TO SV029-STATCDE.                <CS002>
                                                                        <CS002>
           MOVE TV020                  TO WSAA-TABLE.                   <CS002>
           MOVE TRRNRAS-RCSTAT         TO WSAA-ITEM.                    <CS002>
           PERFORM 1800-READ-DESC.                                      <CS002>
           MOVE DESC-LONGDESC          TO SV029-STAT.                   <CS002>
                                                                        <CS002>
           MOVE TRRNRAS-EXPDTE         TO SV029-EXPDTE.                 <CS002>
           MOVE SPACES                 TO SV029-NEWSTS                  <CS002>
                                          SV029-RSNUPD.                 <CS002>
      *                                                                 <CS002>
      *--  If receipt is assigned, get Policy Number , Agent No         <CS002>
      *                                                                 <CS002>
           PERFORM 3500-READ-RCPY.                                      <CS002>
           MOVE WSAA-AGENTNO           TO SV029-AGENTNO.                <CS002>
           MOVE WSAA-CHDRNUM           TO SV029-POLNO.                  <CS002>
                                                                        <CS002>
       A2107-SADD.                                                      <CS002>
      *                                                                 <CS002>
           MOVE SADD                   TO SCRN-FUNCTION.                <CS002>
                                                                        <CS002>
           CALL 'SV029IO'              USING SCRN-SCREEN-PARAMS         <CS002>
                                             SV029-DATA-AREA            <CS002>
                                             SV029-SUBFILE-AREA.        <CS002>
                                                                        <CS002>
           IF  SCRN-STATUZ             NOT = O-K                        <CS002>
           AND                         NOT = ENDP                       <CS002>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <CS002>
               PERFORM 600-FATAL-ERROR                                  <CS002>
           END-IF.                                                      <CS002>
      *                                                                 <CS002>
       A2108-NEXT.                                                      <CS002>
      *                                                                 <CS002>
           MOVE  NEXTR                 TO TRRNRAS-FUNCTION.             <CS002>
           GO TO A2102-LOOP.                                            <CS002>
      *                                                                 <CS002>
       A2109-EXIT.                                                      <CS002>
           EXIT.                                                        <CS002>
      /                                                                 <CS002>
       B2100-LOAD-UR-RCPT SECTION.                                      <PS041>
      *****************************                                     <PS041>
       B2101-START.                                                     <PS041>
      *                                                                 <PS041>
           INITIALIZE                     TRRNURT-PARAMS.               <PS041>
           MOVE SV029-ENTYNUM          TO TRRNURT-TOPER.                <PS041>
           MOVE WSAA-RCTYPE            TO TRRNURT-RCTYPE.               <PS041>
           MOVE WSAA-RCSERFRM          TO TRRNURT-RCSERNUM.             <PS041>
           MOVE SPACES                 TO TRRNURT-LOCALITY.             <PS041>
           MOVE TRRNURTREC             TO TRRNURT-FORMAT.               <PS041>
           MOVE BEGN                   TO TRRNURT-FUNCTION.             <PS041>
      *                                                                 <PS041>
       B2102-LOOP.                                                      <PS041>
      *                                                                 <PS041>
           CALL 'TRRNURTIO'            USING TRRNURT-PARAMS.            <PS041>
                                                                        <PS041>
           IF  TRRNURT-STATUZ          NOT = O-K                        <PS041>
           AND                         NOT = ENDP                       <PS041>
               MOVE TRRNURT-PARAMS     TO SYSR-PARAMS                   <PS041>
               PERFORM 600-FATAL-ERROR                                  <PS041>
           END-IF.                                                      <PS041>
                                                                        <PS041>
           IF  TRRNURT-STATUZ          = ENDP                           <PS041>
           OR  TRRNURT-TOPER           NOT = SV029-ENTYNUM              <PS041>
           OR  TRRNURT-RCTYPE          NOT = WSAA-RCTYPE                <PS041>
           OR  TRRNURT-RCSERNUM        > WSAA-RCSERTO                   <PS041>
               IF  WSAA-COUNT          = ZEROES                         <PS041>
                   PERFORM 2200-ADD-NULL-SUBFILE                        <PS041>
               END-IF                                                   <PS041>
               GO TO B2109-EXIT                                         <PS041>
           END-IF.                                                      <PS041>
      *                                                                 <PS041>
      *--  Filter status                                                <PS041>
      *                                                                 <PS041>
           MOVE TRRNURT-RCSTAT         TO WSAA-CHECK-STATUS.            <PS041>
           PERFORM 1600-CHECK-STATUS.                                   <PS041>
                                                                        <PS041>
           IF  WSAA-VALID              = 'N'                            <PS041>
               GO TO B2108-NEXT                                         <PS041>
           END-IF.                                                      <PS041>
                                                                        <PS041>
           IF  SV029-FLTSTA            NOT = SPACES                     <PS041>
           AND TRRNURT-RCSTAT          NOT = SV029-FLTSTA               <PS041>
               GO TO B2108-NEXT                                         <PS041>
           END-IF.                                                      <PS041>
      *                                                                 <PS041>
      *--  Check Location                                               <PS041>
      *                                                                 <PS041>
           INITIALIZE                     TV020-TV020-REC.              <PS041>
           MOVE TV020                  TO WSAA-TABLE     .              <PS041>
           MOVE TRRNURT-RCSTAT         TO WSAA-ITEM      .              <PS041>
           PERFORM 1900-READ-TABLE.                                     <PS041>
           MOVE ITEM-GENAREA           TO TV020-TV020-REC.              <PS041>
                                                                        <PS041>
           IF  TV020-KEYLOCN           = 'Y'                            <PS041>
           AND WSAA-OFFCDE             NOT = TRRNURT-LOCALITY           <PS041>
               GO TO B2108-NEXT                                         <PS041>
           END-IF.                                                      <PS041>
                                                                        <PS041>
           ADD 1                       TO WSAA-COUNT.                   <PS041>
           MOVE 'Y'                    TO SV029-PROCFLG.                <PS041>
           MOVE 'Y'                    TO SV029-STATCDE-OUT(PR).        <PS041>
           MOVE 'N'                    TO SV029-STATCDE-OUT(ND).        <PS041>
           MOVE 'N'                    TO SV029-DOCTDESC-OUT(ND)        <PS041>
           MOVE 'N'                    TO SV029-DOCTDESC-OUT(PR)        <PS041>
           MOVE 'N'                    TO SV029-NEWSTS-OUT(ND).         <PS041>
           MOVE 'N'                    TO SV029-NEWSTS-OUT(PR).         <PS041>
           MOVE TRRNURT-RCNUM          TO SV029-RCNUM.                  <PS041>
           MOVE TRRNURT-RCSTAT         TO SV029-STATCDE.                <PS041>
                                                                        <PS041>
           MOVE TV020                  TO WSAA-TABLE.                   <PS041>
           MOVE TRRNURT-RCSTAT         TO WSAA-ITEM.                    <PS041>
           PERFORM 1800-READ-DESC.                                      <PS041>
           MOVE DESC-LONGDESC          TO SV029-STAT.                   <PS041>
                                                                        <PS041>
           MOVE TRRNURT-EXPDTE         TO SV029-EXPDTE.                 <PS041>
           MOVE SPACES                 TO SV029-NEWSTS                  <PS041>
                                          SV029-RSNUPD.                 <PS041>
      *                                                                 <PS041>
      *--  If receipt is assigned, get Policy Number , Agent No         <PS041>
      *                                                                 <PS041>
           PERFORM 3500-READ-RCPY.                                      <PS041>
           MOVE WSAA-AGENTNO           TO SV029-AGENTNO.                <PS041>
           MOVE WSAA-CHDRNUM           TO SV029-POLNO.                  <PS041>
                                                                        <PS041>
       B2107-SADD.                                                      <PS041>
      *                                                                 <PS041>
           MOVE SADD                   TO SCRN-FUNCTION.                <PS041>
                                                                        <PS041>
           CALL 'SV029IO'              USING SCRN-SCREEN-PARAMS         <PS041>
                                             SV029-DATA-AREA            <PS041>
                                             SV029-SUBFILE-AREA.        <PS041>
                                                                        <PS041>
           IF  SCRN-STATUZ             NOT = O-K                        <PS041>
           AND                         NOT = ENDP                       <PS041>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <PS041>
               PERFORM 600-FATAL-ERROR                                  <PS041>
           END-IF.                                                      <PS041>
      *                                                                 <PS041>
       B2108-NEXT.                                                      <PS041>
      *                                                                 <PS041>
           MOVE  NEXTR                 TO TRRNURT-FUNCTION.             <PS041>
           GO TO B2102-LOOP.                                            <PS041>
      *                                                                 <PS041>
       B2109-EXIT.                                                      <PS041>
           EXIT.                                                        <PS041>
      /                                                                 <PS041>
       2200-ADD-NULL-SUBFILE SECTION.                                   <PHLRMS>
      *******************************                                   <PHLRMS>
      *                                                                 <PHLRMS>
       2210-INIT.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE 'Y'                    TO SV029-STATCDE-OUT(ND).        <PHLRMS>
           MOVE 'Y'                    TO SV029-STATCDE-OUT(PR).        <PHLRMS>
           MOVE SPACES                 TO SV029-AGENTNO.                <PHLRMS>
           MOVE VRCM-MAX-DATE          TO SV029-EXPDTE.                 <PHLRMS>
           MOVE SPACES                 TO SV029-NEWSTS.                 <PHLRMS>
           MOVE SPACES                 TO SV029-POLNO.                  <PHLRMS>
           MOVE 'N'                    TO SV029-PROCFLG.                <PHLRMS>
           MOVE SPACES                 TO SV029-RCNUM.                  <PHLRMS>
           MOVE SPACES                 TO SV029-RSNUPD.                 <PHLRMS>
           MOVE SPACES                 TO SV029-STAT.                   <PHLRMS>
           MOVE SPACES                 TO SV029-STATCDE.                <PHLRMS>
           MOVE SPACES                 TO SV029-DOCTDESC.               <PHLRMS>
                                                                        <PHLRMS>
           MOVE 'Y'                    TO SV029-DOCTDESC-OUT (ND).      <PHLRMS>
           MOVE 'Y'                    TO SV029-DOCTDESC-OUT (PR).      <PHLRMS>
           MOVE 'Y'                    TO SV029-NEWSTS-OUT (ND).        <PHLRMS>
           MOVE 'Y'                    TO SV029-NEWSTS-OUT (PR).        <PHLRMS>
                                                                        <PHLRMS>
           MOVE SADD                   TO SCRN-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'SV029IO'           USING SCRN-SCREEN-PARAMS            <PHLRMS>
                                          SV029-DATA-AREA               <PHLRMS>
                                          SV029-SUBFILE-AREA.           <PHLRMS>
                                                                        <PHLRMS>
           IF SCRN-STATUZ           NOT = O-K AND ENDP                  <PHLRMS>
                 MOVE SCRN-STATUZ      TO SYSR-STATUZ                   <PHLRMS>
                 PERFORM 600-FATAL-ERROR                                <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       2290-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       2300-LOAD-OFF-RCPT SECTION.                                      <PHLRMS>
      ****************************                                      <PHLRMS>
      *                                                                 <PHLRMS>
       2310-INIT.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     SV029-SUBFILE-AREA.           <PHLRMS>
                                                                        <PHLRMS>
           MOVE SCLR                   TO SCRN-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'SV029IO'              USING SCRN-SCREEN-PARAMS         <PHLRMS>
                                             SV029-DATA-AREA            <PHLRMS>
                                             SV029-SUBFILE-AREA.        <PHLRMS>
                                                                        <PHLRMS>
           IF  SCRN-STATUZ NOT         = O-K                            <PHLRMS>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE ZEROES                 TO WSAA-COUNT.                   <PHLRMS>
           INITIALIZE                     TRRNLOC-PARAMS.               <PHLRMS>
           MOVE SV029-ENTYNUM          TO TRRNLOC-TOLOC.                <PHLRMS>
           MOVE WSAA-RCTYPE            TO TRRNLOC-RCTYPE.               <PHLRMS>
           MOVE WSAA-RCSERFRM          TO TRRNLOC-RCSERNUM.             <PHLRMS>
           MOVE TRRNLOCREC             TO TRRNLOC-FORMAT.               <PHLRMS>
           MOVE BEGN                   TO TRRNLOC-FUNCTION.             <PHLRMS>
      *                                                                 <PHLRMS>
       2320-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           CALL 'TRRNLOCIO'            USING TRRNLOC-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF TRRNLOC-STATUZ           NOT = O-K                        <PHLRMS>
           AND                         NOT = ENDP                       <PHLRMS>
               MOVE TRRNLOC-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE TRRNLOC-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNLOC-STATUZ          = ENDP                           <PHLRMS>
           OR  TRRNLOC-TOLOC           NOT = SV029-ENTYNUM              <PHLRMS>
           OR  TRRNLOC-RCTYPE          NOT = WSAA-RCTYPE                <PHLRMS>
           OR  TRRNLOC-RCSERNUM        > WSAA-RCSERTO                   <PHLRMS>
               IF  WSAA-COUNT          = ZEROES                         <PHLRMS>
                   PERFORM 2200-ADD-NULL-SUBFILE                        <PHLRMS>
               END-IF                                                   <PHLRMS>
               GO TO 2390-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Filter status                                                <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE TRRNLOC-RCSTAT         TO WSAA-CHECK-STATUS.            <PHLRMS>
           PERFORM 1600-CHECK-STATUS.                                   <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-VALID              = 'N'                            <PHLRMS>
               GO TO 2380-NEXT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  SV029-FLTSTA            NOT = SPACES                     <PHLRMS>
           AND TRRNLOC-RCSTAT          NOT = SV029-FLTSTA               <PHLRMS>
               GO TO 2380-NEXT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Check Location                                               <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     TV020-TV020-REC.              <PHLRMS>
           MOVE TV020                  TO WSAA-TABLE     .              <PHLRMS>
           MOVE TRRNLOC-RCSTAT         TO WSAA-ITEM      .              <PHLRMS>
           PERFORM 1900-READ-TABLE.                                     <PHLRMS>
           MOVE ITEM-GENAREA           TO TV020-TV020-REC.              <PHLRMS>
                                                                        <PHLRMS>
           IF  TV020-KEYLOCN           = 'Y'                            <PHLRMS>
           AND WSAA-OFFCDE             NOT = TRRNLOC-LOCALITY           <PHLRMS>
               GO TO 2380-NEXT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           ADD 1                       TO WSAA-COUNT.                   <PHLRMS>
           MOVE 'Y'                    TO SV029-PROCFLG.                <PHLRMS>
           MOVE 'Y'                    TO SV029-STATCDE-OUT(PR).        <PHLRMS>
           MOVE 'N'                    TO SV029-STATCDE-OUT(ND).        <PHLRMS>
           MOVE 'N'                    TO SV029-DOCTDESC-OUT(ND).       <PHLRMS>
           MOVE 'N'                    TO SV029-DOCTDESC-OUT(PR).       <PHLRMS>
           MOVE 'N'                    TO SV029-NEWSTS-OUT (ND).        <PHLRMS>
           MOVE 'N'                    TO SV029-NEWSTS-OUT (PR).        <PHLRMS>
           MOVE TRRNLOC-RCNUM          TO SV029-RCNUM.                  <PHLRMS>
           MOVE TRRNLOC-RCSTAT         TO SV029-STATCDE.                <PHLRMS>
                                                                        <PHLRMS>
           MOVE TV020                  TO WSAA-TABLE.                   <PHLRMS>
           MOVE TRRNLOC-RCSTAT         TO WSAA-ITEM.                    <PHLRMS>
           PERFORM 1800-READ-DESC.                                      <PHLRMS>
           MOVE DESC-LONGDESC          TO SV029-STAT.                   <PHLRMS>
                                                                        <PHLRMS>
           MOVE TRRNLOC-EXPDTE         TO SV029-EXPDTE.                 <PHLRMS>
           MOVE SPACES                 TO SV029-NEWSTS                  <PHLRMS>
                                          SV029-RSNUPD.                 <PHLRMS>
      *                                                                 <PHLRMS>
      *--  If receipt is assigned, get Policy Number , Agent No         <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM 3500-READ-RCPY.                                      <PHLRMS>
           MOVE WSAA-AGENTNO           TO SV029-AGENTNO.                <PHLRMS>
           MOVE WSAA-CHDRNUM           TO SV029-POLNO.                  <PHLRMS>
      *                                                                 <PHLRMS>
       2370-SADD.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE SADD                   TO SCRN-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'SV029IO'              USING SCRN-SCREEN-PARAMS         <PHLRMS>
                                             SV029-DATA-AREA            <PHLRMS>
                                             SV029-SUBFILE-AREA.        <PHLRMS>
                                                                        <PHLRMS>
           IF  SCRN-STATUZ             NOT = O-K AND ENDP               <PHLRMS>
               MOVE SCRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       2380-NEXT.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE  NEXTR                 TO TRRNLOC-FUNCTION.             <PHLRMS>
           GO TO 2320-START.                                            <PHLRMS>
      *                                                                 <PHLRMS>
       2390-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /
       2600-VALIDATE-SUBFILE SECTION.
      *******************************
      *
       2610-VALIDATION.
      *
      *--  Validate subfile fields
      *
           IF  SV029-PROCFLG           = 'N'                            <PHLRMS>
               GO TO 2680-READ-NEXT-MODIFIED-RECORD                     <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Re-read TRRNINF to validate whether status was changed       <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     TRRNINF-PARAMS.               <PHLRMS>
           MOVE SV029-RCNUM (1:2)      TO TRRNINF-RCTYPE.               <PHLRMS>
           MOVE SV029-RCNUM (3:8)      TO TRRNINF-RCSERNUM.             <PHLRMS>
           MOVE TRRNINFREC             TO TRRNINF-FORMAT.               <PHLRMS>
           MOVE READR                  TO TRRNINF-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'TRRNINFIO'            USING TRRNINF-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNINF-STATUZ          NOT = O-K                        <PHLRMS>
               MOVE TRRNINF-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE TRRNINF-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE TRRNINF-RCSTAT         TO WSAA-CHECK-STATUS.            <PHLRMS>
           PERFORM 1600-CHECK-STATUS.                                   <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-VALID              = 'N'                            <PHLRMS>
               MOVE EV24               TO SV029-NEWSTS-ERR              <PHLRMS>
               GO TO 2670-UPDATE-ERROR-INDICATORS                       <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Fill all status to subfile                                   <PHLRMS>
      *                                                                 <PHLRMS>
           IF  SV029-FILSTS             NOT = SPACES                    <PHLRMS>
               MOVE SV029-FILSTS       TO SV029-NEWSTS                  <PHLRMS>
           END-IF.

           IF  SV029-FLTRSN             NOT = SPACES                    <PHLRMS>
               MOVE SV029-FLTRSN       TO SV029-RSNUPD                  <PHLRMS>
           END-IF.

      ***************************************************************
      *  Special case : allow update Lost - Return in case Agent    *
      *  finds the losted receipt                                   *
      ***************************************************************
      ***  IF  SV029-STATCDE           = 'LS'                           <PHLRMS>
      ***  AND SV029-NEWSTS            NOT = SPACES                     <PHLRMS>
      ***  AND SV029-NEWSTS            NOT = 'LR'                       <PHLRMS>
      ***      MOVE EV24               TO SV029-NEWSTS-ERR              <PHLRMS>
      ***      GO TO 2670-UPDATE-ERROR-INDICATORS                       <PHLRMS>
      ***  END-IF.                                                      <PHLRMS>

      ***  IF  SV029-NEWSTS            = 'LR'                           <PHLRMS>
      ***  AND SV029-STATCDE           NOT = 'LS'                       <PHLRMS>
      ***      MOVE EV24               TO SV029-NEWSTS-ERR              <PHLRMS>
      ***      GO TO 2670-UPDATE-ERROR-INDICATORS                       <PHLRMS>
      ***  END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Validate Status for update                                   <PHLRMS>
      *                                                                 <PHLRMS>
           IF  SV029-NEWSTS            = SPACES                         <PHLRMS>
           AND SV029-RSNUPD            NOT = SPACES                     <PHLRMS>
               MOVE H366               TO SV029-NEWSTS-ERR              <PHLRMS>
               GO TO 2670-UPDATE-ERROR-INDICATORS
           END-IF.
                                                                        <PHLRMS>
           IF  SV029-NEWSTS            NOT = SPACES                     <PHLRMS>
               INITIALIZE                 TV020-TV020-REC               <PHLRMS>
               MOVE TV020              TO WSAA-TABLE                    <PHLRMS>
               MOVE SV029-STATCDE      TO WSAA-ITEM                     <PHLRMS>
               PERFORM 1900-READ-TABLE                                  <PHLRMS>
               MOVE ITEM-GENAREA       TO TV020-TV020-REC               <PHLRMS>
                                                                        <PHLRMS>
               MOVE 'N'                TO WSAA-FOUND                    <PHLRMS>
               PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 20           <PHLRMS>
                         OR WSAA-FOUND = 'Y'                            <PHLRMS>
                         OR TV020-RCSTAT (IDX) = SPACES                 <PHLRMS>
                   IF  TV020-RCSTAT (IDX) = SV029-NEWSTS                <PHLRMS>
                       MOVE 'Y'        TO WSAA-FOUND                    <PHLRMS>
                       MOVE IDX        TO WSAA-CURSOR-STAT              <PHLRMS>
                   END-IF                                               <PHLRMS>
               END-PERFORM                                              <PHLRMS>
                                                                        <PHLRMS>
               IF  WSAA-FOUND          = 'N'                            <PHLRMS>
                   MOVE EV24           TO SV029-NEWSTS-ERR              <PHLRMS>
                   GO TO 2670-UPDATE-ERROR-INDICATORS                   <PHLRMS>
      ***      ELSE                                                     <PHLRMS>
      ***          IF  TV020-KEYLOCN(WSAA-CURSOR-STAT) = 'Y'            <PHLRMS>
      ***          AND WSAA-OFFCDE     NOT = TRRNINF-LOCALITY           <PHLRMS>
      ***              MOVE EV57       TO SV029-NEWSTS-ERR              <PHLRMS>
      ***              GO TO 2670-UPDATE-ERROR-INDICATORS               <PHLRMS>
      ***          END-IF                                               <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           INITIALIZE                     TV020-TV020-REC.              <PHLRMS>
           MOVE TV020                  TO WSAA-TABLE.                   <PHLRMS>
           MOVE SV029-NEWSTS           TO WSAA-ITEM.                    <PHLRMS>
           PERFORM 1900-READ-TABLE.                                     <PHLRMS>
           MOVE ITEM-GENAREA           TO TV020-TV020-REC.              <PHLRMS>
                                                                        <PHLRMS>
           IF  TV020-RSCDES            NOT = SPACES                     <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Validate Reason for update                                   <PHLRMS>
      *                                                                 <PHLRMS>
               IF  SV029-RSNUPD        = SPACES                         <PHLRMS>
                   MOVE E186           TO SV029-RSNUPD-ERR              <PHLRMS>
                   GO TO 2670-UPDATE-ERROR-INDICATORS                   <PHLRMS>
               ELSE                                                     <PHLRMS>
                   PERFORM 2700-CHECK-REASON                            <PHLRMS>
                   IF  WSAA-FOUND      = 'N'                            <PHLRMS>
                       MOVE RLCF       TO SV029-RSNUPD-ERR              <PHLRMS>
                       GO TO 2670-UPDATE-ERROR-INDICATORS               <PHLRMS>
                   END-IF                                               <PHLRMS>
               END-IF                                                   <PHLRMS>
           ELSE                                                         <PHLRMS>
               IF  SV029-RSNUPD        NOT = SPACES                     <PHLRMS>
                   MOVE E492           TO SV029-RSNUPD-ERR              <PHLRMS>
                   GO TO 2670-UPDATE-ERROR-INDICATORS                   <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Validate Reason for Key-in Remark                            <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM 2800-CHECK-KEYIN-REMARK.                             <PHLRMS>
           IF  WSAA-FOUND              = 'Y'                            <PHLRMS>
               IF  SV029-DOCTDESC      = SPACES                         <PHLRMS>
                   MOVE E186           TO SV029-DOCTDESC-ERR            <PHLRMS>
               END-IF                                                   <PHLRMS>
           ELSE                                                         <PHLRMS>
               IF  SV029-DOCTDESC      NOT = SPACES                     <PHLRMS>
                   MOVE E492           TO SV029-DOCTDESC-ERR            <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *
       2670-UPDATE-ERROR-INDICATORS.
      *
           IF SV029-ERROR-SUBFILE      NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.

           MOVE SUPD                   TO SCRN-FUNCTION.

           CALL 'SV029IO' USING SCRN-SCREEN-PARAMS
                                SV029-DATA-AREA
                                SV029-SUBFILE-AREA.
           IF SCRN-STATUZ              NOT = O-K
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
       2680-READ-NEXT-MODIFIED-RECORD.
      *
           MOVE SRDN                   TO SCRN-FUNCTION.

           CALL 'SV029IO' USING SCRN-SCREEN-PARAMS
                                SV029-DATA-AREA
                                SV029-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
                                   AND NOT = ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
       2690-EXIT.
            EXIT.
      /                                                                 <PHLRMS>
       2700-CHECK-REASON SECTION.                                       <PHLRMS>
      ***************************                                       <PHLRMS>
      *                                                                 <PHLRMS>
       2710-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE 'N'                TO WSAA-FOUND.                       <PHLRMS>
                                                                        <PHLRMS>
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 8                <PHLRMS>
                        OR WSAA-FOUND  = 'Y'                            <PHLRMS>
               IF TV020-RSCDE (IDX)    = SV029-RSNUPD                   <PHLRMS>
                   MOVE 'Y'            TO WSAA-FOUND                    <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-PERFORM.                                                 <PHLRMS>
      *                                                                 <PHLRMS>
       2790-EXIT.                                                       <PHLRMS>
            EXIT.                                                       <PHLRMS>
      /                                                                 <PHLRMS>
       2800-CHECK-KEYIN-REMARK SECTION.                                 <PHLRMS>
      *********************************                                 <PHLRMS>
      *                                                                 <PHLRMS>
       2810-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE 'N'                TO WSAA-FOUND.                       <PHLRMS>
                                                                        <PHLRMS>
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 20               <PHLRMS>
                                 OR TV037-RMKSTAT (IDX) = SPACES        <PHLRMS>
                        OR WSAA-FOUND  = 'Y'                            <PHLRMS>
               IF TV037-RMKSTAT (IDX)  = SV029-NEWSTS                   <PHLRMS>
                   MOVE 'Y'            TO WSAA-FOUND                    <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-PERFORM.                                                 <PHLRMS>
      *                                                                 <PHLRMS>
       2890-EXIT.                                                       <PHLRMS>
            EXIT.                                                       <PHLRMS>
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
           MOVE ZEROES                 TO WSAA-COUNT-PRINT.             <PHLRMS>
      *                                                                 <PHLRMS>
      *  Update database files as required
      *
           MOVE SSTRT                  TO SCRN-FUNCTION.

           CALL 'SV029IO'              USING SCRN-SCREEN-PARAMS
                                             SV029-DATA-AREA
                                             SV029-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
           AND                         NOT = ENDP
              MOVE SCRN-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           PERFORM 3700-GET-NEXT-SEQ.                                   <PHLRMS>
                                                                        <PHLRMS>
           PERFORM 3100-LOOP-SUBFILE   UNTIL SCRN-STATUZ = ENDP.
                                                                        <PHLRMS>
           IF  SV029-DENTTYP           = 'AG'                           <PHLRMS>
           AND WSAA-COUNT-PRINT        > 0                              <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Print PDF                                                    <PHLRMS>
      *                                                                 <PHLRMS>
               PERFORM 3600-PRINT                                       <PHLRMS>
TDO   * Popup PDF:                                                      <CS006>
               PERFORM X100-POPUP-PDF-FILE                              <CS006>
                                                                        <CS006>
           END-IF.                                                      <PHLRMS>
      *
       3090-EXIT.
            EXIT.
      /
       3100-LOOP-SUBFILE  SECTION.
      ****************************
      *
       3110-START.
      *
           IF  SV029-PROCFLG           = 'N'
               GO TO 3180-NEXT-SUBFILE                                  <PHLRMS>
           END-IF.

           IF  SV029-NEWSTS            = SPACES                         <PHLRMS>
               GO TO 3180-NEXT-SUBFILE                                  <PHLRMS>
           END-IF.

           PERFORM 3200-UPDATE-TRRN.                                    <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Check if status and reason will print-out the confirmation   <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM 4200-READ-TV058.                                     <PHLRMS>
                                                                        <PHLRMS>
           IF  SV029-DENTTYP           = 'AG'                           <PHLRMS>
               PERFORM 4300-CHECK-PRINT                                 <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *
       3180-NEXT-SUBFILE.                                               <PHLRMS>
      *
      *-- Next record
      *
           MOVE SRDN                   TO SCRN-FUNCTION.

           CALL 'SV029IO' USING SCRN-SCREEN-PARAMS
                                SV029-DATA-AREA
                                SV029-SUBFILE-AREA.

           IF SCRN-STATUZ              NOT = O-K
           AND                         NOT = ENDP
               MOVE SCRN-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       3190-EXIT.
           EXIT.
      /
       3200-UPDATE-TRRN SECTION.
      **************************
      *
       3210-START.                                                      <PHLRMS>
      *
           INITIALIZE                     TRRN-PARAMS.                  <PHLRMS>
           MOVE SV029-RCNUM(1:2)       TO TRRN-RCTYPE.                  <PHLRMS>
           MOVE SV029-RCNUM(3:8)       TO TRRN-RCSERNUM.                <PHLRMS>
           MOVE TRRNREC                TO TRRN-FORMAT.                  <PHLRMS>
           MOVE READR                  TO TRRN-FUNCTION.                <PHLRMS>
           CALL 'TRRNIO'               USING TRRN-PARAMS.               <PHLRMS>

           IF  TRRN-STATUZ             NOT = O-K                        <PHLRMS>
               MOVE TRRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               MOVE TRRN-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
      *--  Update current record, set validflag = '2'                   <PHLRMS>
      *
           MOVE '2'                    TO TRRN-VALIDFLAG.               <PHLRMS>
           MOVE WRITD                  TO TRRN-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'TRRNIO'               USING TRRN-PARAMS.               <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRN-STATUZ             NOT = O-K                        <PHLRMS>
               MOVE TRRN-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               MOVE TRRN-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
      *--  Write a new record with validflag = '1'                      <PHLRMS>
      *
           MOVE ZEROES                 TO WSAA-TRANNO.
           MOVE TRRN-RCSERNUM          TO WSAA-RCSERNUM.                <PHLRMS>
           PERFORM 3300-GET-NEW-TRANNO.
           MOVE WSAA-TRANNO            TO TRRN-TRANNO.

           MOVE SV029-NEWSTS           TO TRRN-RCSTAT.                  <PHLRMS>
      **** IF SV029-NEWSTS             = 'UR'                           <PS041>
      ****    MOVE 'UR'               TO TRRN-STATCODE                  <PS041>
      **** END-IF.                                                      <PS041>
                                                                        <PS041>
           IF SV029-NEWSTS             = 'UR'                           <PS041>
              MOVE 'RT'               TO TRRN-STATCODE                  <PS041>
           END-IF.                                                      <PS041>
                                                                        <PS041>
           IF  SV029-STATCDE           = 'AS'                           <PHLRMS>
           AND SV029-NEWSTS            NOT = 'UR'                       <PS041>
      *                                                                 <PHLRMS>
      *--  Status                                                       <PHLRMS>
      *                                                                 <PHLRMS>
               MOVE 'RT'               TO TRRN-STATCODE                 <PHLRMS>
      *                                                                 <PHLRMS>
               MOVE TRRN-TOPFX         TO WSAA-FRMPFX                   <PHLRMS>
               MOVE TRRN-TOPER         TO WSAA-FROMPER                  <PHLRMS>
               MOVE TRRN-FRMPFX        TO WSAA-TOPFX                    <PHLRMS>
               MOVE TRRN-FROMLOC       TO WSAA-TOLOC                    <PHLRMS>
      *                                                                 <PHLRMS>
      *--  From                                                         <PHLRMS>
      *                                                                 <PHLRMS>
               MOVE WSAA-FRMPFX        TO TRRN-FRMPFX                   <PHLRMS>
               MOVE WSAA-FROMPER       TO TRRN-FROMPER                  <PHLRMS>
               MOVE SPACES             TO TRRN-FROMLOC                  <PHLRMS>
      *                                                                 <PHLRMS>
      *--  To                                                           <PHLRMS>
      *                                                                 <PHLRMS>
               MOVE WSAA-TOPFX         TO TRRN-TOPFX                    <PHLRMS>
               MOVE WSAA-TOLOC         TO TRRN-TOLOC                    <PHLRMS>
               MOVE SPACES             TO TRRN-TOPER                    <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <CS001>
           IF  SV029-STATCDE           = 'RA'                           <CS002>
           AND SV029-NEWSTS            NOT = 'UR'                       <PS041>
      *                                                                 <CS002>
      *--  Status                                                       <CS002>
      *                                                                 <CS002>
               MOVE 'RT'               TO TRRN-STATCODE                 <CS002>
      *                                                                 <CS002>
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
                                                                        <CS002>
           IF WSSP-SBMACTION           = 'O'                            <CS001>
           AND SV029-STATCDE           = 'CA'                           <CS001>
      *--  Status                                                       <CS001>
                                                                        <CS001>
               MOVE 'AS'               TO TRRN-STATCODE                 <CS001>
                                                                        <CS001>
               MOVE TRRN-TOPFX         TO WSAA-FRMPFX                   <CS001>
               MOVE TRRN-FRMPFX        TO WSAA-TOPFX                    <CS001>
               MOVE TRRN-TOLOC         TO WSAA-FROMLOC                  <CS001>
               MOVE TRRN-FROMPER       TO WSAA-TOPER                    <CS001>
               MOVE TRRN-TOPER         TO WSAA-FROMPER                  <CS002>
      *                                                                 <CS001>
      *--  From                                                         <CS001>
      *                                                                 <CS001>
               MOVE WSAA-FRMPFX        TO TRRN-FRMPFX                   <CS001>
               MOVE WSAA-FROMLOC       TO TRRN-FROMLOC                  <CS001>
               MOVE SPACES             TO TRRN-FROMPER                  <CS002>
                                                                        <CS001>
      *--  To                                                           <CS001>
      *                                                                 <CS001>
               MOVE WSAA-TOPER         TO TRRN-TOPER                    <CS001>
               MOVE WSAA-TOPFX         TO TRRN-TOPFX                    <CS001>
               MOVE SPACES             TO TRRN-TOLOC                    <CS001>
                                                                        <CS001>
               PERFORM A3200-DELETE-RECEIPT                             <CS001>
           END-IF.                                                      <CS001>
                                                                        <PS041>
           IF SV029-STATCDE            = 'UR'                           <PS041>
      *--  Status                                                       <PS041>
      *                                                                 <PS041>
               MOVE 'RT'               TO TRRN-STATCODE                 <PS041>
      *                                                                 <PS041>
               MOVE TRRN-TOPFX         TO WSAA-FRMPFX                   <PS041>
               MOVE TRRN-TOPER         TO WSAA-FROMPER                  <PS041>
               MOVE TRRN-FRMPFX        TO WSAA-TOPFX                    <PS041>
               MOVE TRRN-FROMLOC       TO WSAA-TOLOC                    <PS041>
      *                                                                 <PS041>
      *--  From                                                         <PS041>
      *                                                                 <PS041>
               MOVE WSAA-FRMPFX        TO TRRN-FRMPFX                   <PS041>
               MOVE WSAA-FROMPER       TO TRRN-FROMPER                  <PS041>
               MOVE SPACES             TO TRRN-FROMLOC                  <PS041>
      *                                                                 <PS041>
      *--  To                                                           <PS041>
      *                                                                 <PS041>
               MOVE WSAA-TOPFX         TO TRRN-TOPFX                    <PS041>
               MOVE WSAA-TOLOC         TO TRRN-TOLOC                    <PS041>
               MOVE SPACES             TO TRRN-TOPER                    <PS041>
           END-IF.                                                      <PS041>
                                                                        <PHLRMS>
           MOVE SV029-RSNUPD           TO TRRN-RSCDE.                   <PHLRMS>
           MOVE SV029-DOCTDESC         TO TRRN-DOCTDESC.                <PHLRMS>
           MOVE WSKY-BATC-BATCTRCDE    TO TRRN-TRANCDE.
           MOVE '1'                    TO TRRN-VALIDFLAG.
           MOVE WSAA-TODAY             TO TRRN-TRANDATE.
           MOVE WSSP-USERID            TO TRRN-CRUSER.
           MOVE WSAA-OFFCDE            TO TRRN-OFFPRC.                  <PHLRMS>
           MOVE WRITR                  TO TRRN-FUNCTION.

           CALL 'TRRNIO'               USING TRRN-PARAMS.

           IF  TRRN-STATUZ             NOT = O-K
               MOVE TRRN-STATUZ        TO SYSR-STATUZ
               MOVE TRRN-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       3290-EXIT.
            EXIT.
      /
       A3200-DELETE-RECEIPT SECTION.                                    <CS001>
      *****************************                                     <CS001>
       A3201-START.                                                     <CS001>
      *                                                                 <CS001>
            INITIALIZE                  ZPPRCHK-PARAMS.                 <CS001>
            MOVE SV029-STATCDE          TO ZPPRCHK-PREFIX.              <CS001>
            MOVE WSSP-COMPANY           TO ZPPRCHK-COMPANY.             <CS001>
            MOVE SV029-RCNUM(1:2)       TO ZPPRCHK-RCPREPRTCD.          <CS001>
            MOVE SV029-RCNUM(3:8)       TO ZPPRCHK-RCPREPRNT.           <CS001>
            MOVE ZPPRCHKREC             TO ZPPRCHK-FORMAT.              <CS001>
            MOVE READR                  TO ZPPRCHK-FUNCTION.            <CS001>
                                                                        <CS001>
            CALL 'ZPPRCHKIO'            USING ZPPRCHK-PARAMS.           <CS001>
                                                                        <CS001>
            IF ZPPRCHK-STATUZ           NOT = O-K                       <CS001>
            AND                         NOT = MRNF                      <CS001>
               MOVE ZPPRCHK-PARAMS     TO SYSR-PARAMS                   <CS001>
               PERFORM 600-FATAL-ERROR                                  <CS001>
            END-IF.                                                     <CS001>
                                                                        <CS001>
            IF ZPPRCHK-STATUZ           = O-K                           <CS001>
               MOVE DELTD               TO ZPPRCHK-FUNCTION             <CS001>
               CALL 'ZPPRCHKIO'         USING ZPPRCHK-PARAMS            <CS001>
                                                                        <CS001>
               IF  ZPPRCHK-STATUZ       NOT = O-K                       <CS001>
                   MOVE ZPPRCHK-PARAMS  TO SYSR-PARAMS                  <CS001>
                   PERFORM 600-FATAL-ERROR                              <CS001>
               END-IF                                                   <CS001>
            END-IF.                                                     <CS001>
      *                                                                 <CS001>
       A3209-EXIT.                                                      <CS001>
            EXIT.                                                       <CS001>
      /                                                                 <CS001>
       3300-GET-NEW-TRANNO SECTION.
      *****************************
      *
       3310-START.
      *
           INITIALIZE                     TRRNTRN-PARAMS.
           MOVE WSAA-RCTYPE            TO TRRNTRN-RCTYPE.
           MOVE WSAA-RCSERNUM          TO TRRNTRN-RCSERNUM.
           MOVE 99999                  TO TRRNTRN-TRANNO.
           MOVE TRRNTRNREC             TO TRRNTRN-FORMAT.
           MOVE BEGN                   TO TRRNTRN-FUNCTION.

           CALL 'TRRNTRNIO'            USING TRRNTRN-PARAMS.

           IF TRRNTRN-STATUZ           NOT = O-K
           AND                         NOT = ENDP
               MOVE TRRNTRN-STATUZ     TO SYSR-STATUZ
               MOVE TRRNTRN-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF TRRNTRN-STATUZ           NOT = O-K
           OR TRRNTRN-RCTYPE           NOT = WSAA-RCTYPE
           OR TRRNTRN-RCSERNUM         NOT = WSAA-RCSERNUM
               MOVE 1                  TO WSAA-TRANNO
               GO TO 3390-EXIT
           END-IF.

           COMPUTE WSAA-TRANNO         = TRRNTRN-TRANNO + 1.
      *
       3390-EXIT.
           EXIT.
      /                                                                 <PHLRMS>
       3400-READ-TV020 SECTION.                                         <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       3410-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     TV020-TV020-REC.              <PHLRMS>
           MOVE TV020                  TO WSAA-TABLE.                   <PHLRMS>
           MOVE SV029-STATCDE          TO WSAA-ITEM.                    <PHLRMS>
           PERFORM 1900-READ-TABLE.                                     <PHLRMS>
           MOVE ITEM-GENAREA           TO TV020-TV020-REC.              <PHLRMS>
      *                                                                 <PHLRMS>
       3490-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /
       3500-READ-RCPY SECTION.
      ************************
      *
       3510-START.
      *
           MOVE SPACES                 TO WSAA-AGENTNO.                 <PHLRMS>
           MOVE SPACES                 TO WSAA-CHDRNUM.
           INITIALIZE                     RCPYACT-PARAMS.
           MOVE SV029-RCNUM(1:2)       TO RCPYACT-RCTYPE.
           MOVE SV029-RCNUM(3:8)       TO RCPYACT-RCSERNUM.
           MOVE RCPYACTREC             TO RCPYACT-FORMAT.
           MOVE READR                  TO RCPYACT-FUNCTION.

           CALL 'RCPYACTIO'            USING RCPYACT-PARAMS.

           IF  RCPYACT-STATUZ          NOT = O-K
           AND                         NOT = MRNF
              MOVE RCPYACT-STATUZ      TO SYSR-STATUZ
              MOVE RCPYACT-PARAMS      TO SYSR-PARAMS
              GO TO 600-FATAL-ERROR
           END-IF.

           IF  RCPYACT-STATUZ          = O-K
               MOVE RCPYACT-CHDRNUM    TO WSAA-CHDRNUM
               MOVE RCPYACT-ENTYNUM    TO WSAA-AGENTNO                  <PHLRMS>
           END-IF.
      *
       3590-EXIT.
           EXIT.
      /                                                                 <PHLRMS>
       3600-PRINT SECTION.                                              <PHLRMS>
      ********************                                              <PHLRMS>
      *                                                                 <PHLRMS>
       3610-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Insert list of Agent into record in LETC                     <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM 3800-ADD-LETC.                                       <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Delete temp record in RETR                                   <PHLRMS>
      *                                                                 <PHLRMS>
           PERFORM 3900-DELT-RETR.                                      <PHLRMS>
      *                                                                 <PHLRMS>
       3690-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       3700-GET-NEXT-SEQ SECTION.                                       <PHLRMS>
      ***************************                                       <PHLRMS>
      *                                                                 <PHLRMS>
       3710-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     RETRTRN-PARAMS.               <PHLRMS>
           MOVE 9999999                TO RETRTRN-SEQNUM.               <PHLRMS>
           MOVE RETRTRNREC             TO RETRTRN-FORMAT.               <PHLRMS>
           MOVE BEGN                   TO RETRTRN-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'RETRTRNIO'            USING RETRTRN-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  RETRTRN-STATUZ          NOT = O-K                        <PHLRMS>
           AND                         NOT = ENDP                       <PHLRMS>
               MOVE RETRTRN-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               MOVE RETRTRN-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  RETRTRN-STATUZ          = ENDP                           <PHLRMS>
               MOVE 1                  TO WSAA-SEQNUM                   <PHLRMS>
               GO TO 3790-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           COMPUTE WSAA-SEQNUM         = RETRTRN-SEQNUM  +  1.          <PHLRMS>
      *                                                                 <PHLRMS>
       3790-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       3800-ADD-LETC SECTION.                                           <PHLRMS>
      ***********************                                           <PHLRMS>
      *                                                                 <PHLRMS>
       3810-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     ITEM-PARAMS.                  <PHLRMS>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <PHLRMS>
           MOVE WSSP-COMPANY           TO ITEM-ITEMCOY.                 <PHLRMS>
           MOVE TR384                  TO ITEM-ITEMTABL.                <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Build key to TR384 from '****'  & transaction code.          <PHLRMS>
      *                                                                 <PHLRMS>
           STRING             '***' WSKY-BATC-BATCTRCDE                 <PHLRMS>
           DELIMITED BY       SPACE                                     <PHLRMS>
           INTO               ITEM-ITEMITEM.                            <PHLRMS>
                                                                        <PHLRMS>
           MOVE READR                  TO ITEM-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <PHLRMS>
                                                                        <PHLRMS>
           IF  ITEM-STATUZ             NOT = O-K                        <PHLRMS>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               MOVE G437               TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE ITEM-GENAREA           TO TR384-TR384-REC.              <PHLRMS>
                                                                        <PHLRMS>
           IF  TR384-LETTER-TYPE       = SPACES                         <PHLRMS>
               MOVE 'NTYP'             TO SYSR-STATUZ                   <PHLRMS>
               PERFORM  600-FATAL-ERROR                                 <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Write LETC via LETRQST.                                      <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE SPACES                 TO LETRQST-STATUZ.               <PHLRMS>
           MOVE WSSP-COMPANY           TO LETRQST-REQUEST-COMPANY.      <PHLRMS>
           MOVE TR384-LETTER-TYPE      TO LETRQST-LETTER-TYPE.          <PHLRMS>
           MOVE WSAA-TODAY             TO LETRQST-LETTER-REQUEST-DATE.  <PHLRMS>
           MOVE AGNTLAG-CLNTCOY        TO LETRQST-CLNTCOY.              <PHLRMS>
           MOVE AGNTLAG-CLNTNUM        TO LETRQST-CLNTNUM.              <PHLRMS>
                                                                        <PHLRMS>
           MOVE AGNTLAG-AGNTPFX        TO LETRQST-RDOCPFX.              <PHLRMS>
           MOVE AGNTLAG-AGNTCOY        TO LETRQST-RDOCCOY.              <PHLRMS>
           MOVE AGNTLAG-AGNTNUM        TO LETRQST-RDOCNUM.              <PHLRMS>
           MOVE SPACES                 TO LETRQST-CHDRCOY.              <PHLRMS>
           MOVE SPACES                 TO LETRQST-CHDRNUM.              <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Key for get printing data                                    <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE WSAA-SEQNUM            TO LETRQST-TRANNO.               <PHLRMS>
           MOVE SPACES                 TO LETRQST-OTHER-KEYS.           <PHLRMS>
           MOVE SPACES                 TO LETRQST-BRANCH.               <PHLRMS>
                                                                        <PHLRMS>
           MOVE 'ADD'                  TO LETRQST-FUNCTION.             <PHLRMS>
      *                                                                 <PHLRMS>
           IF  TR384-LETTER-TYPE       NOT = SPACES                     <PHLRMS>
                                                                        <PHLRMS>
               CALL 'LETRQST' USING LETRQST-PARAMS                      <PHLRMS>
      *                                                                 <PHLRMS>
               IF  LETRQST-STATUZ      NOT = O-K                        <PHLRMS>
                   MOVE LETRQST-PARAMS TO SYSR-PARAMS                   <PHLRMS>
                   MOVE LETRQST-STATUZ TO SYSR-STATUZ                   <PHLRMS>
                   PERFORM 600-FATAL-ERROR                              <PHLRMS>
               END-IF                                                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       3890-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      *                                                                 <PHLRMS>
       3900-DELT-RETR SECTION.                                          <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       3910-INIT.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     RETR-PARAMS.                  <PHLRMS>
           MOVE WSAA-SEQNUM            TO RETR-SEQNUM.                  <PHLRMS>
           MOVE SPACES                 TO RETR-RCNUM.                   <PHLRMS>
           MOVE RETRREC                TO RETR-FORMAT.                  <PHLRMS>
           MOVE BEGN                   TO RETR-FUNCTION.                <PHLRMS>
      *                                                                 <PHLRMS>
       3920-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           CALL 'RETRIO'               USING RETR-PARAMS.               <PHLRMS>
                                                                        <PHLRMS>
           IF  RETR-STATUZ             NOT = O-K                        <PHLRMS>
           AND                         NOT = ENDP                       <PHLRMS>
               MOVE RETR-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               MOVE RETR-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  RETR-STATUZ             = ENDP                           <PHLRMS>
           OR  RETR-SEQNUM             NOT = WSAA-SEQNUM                <PHLRMS>
               GO TO 3990-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           MOVE DELTD                  TO RETR-FUNCTION.                <PHLRMS>
           CALL 'RETRIO'               USING RETR-PARAMS.               <PHLRMS>
                                                                        <PHLRMS>
           IF  RETR-STATUZ             NOT = O-K                        <PHLRMS>
               MOVE RETR-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               MOVE RETR-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       3980-NEXT.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE NEXTR                  TO RETR-FUNCTION.                <PHLRMS>
           GO TO 3920-START.                                            <PHLRMS>
      *                                                                 <PHLRMS>
       3990-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /
      *****************************************************************
      *     DECIDE WHICH TRANSACTION PROGRAM IS NEXT
      *****************************************************************
      *
       4000-WHERE-NEXT SECTION.
      *************************
      *
       4010-NEXT-PROGRAM.
      *
           ADD 1                       TO WSSP-PROGRAM-PTR.
      *
       4090-EXIT.
            EXIT.
      /                                                                 <PHLRMS>
       4200-READ-TV058 SECTION.                                         <PHLRMS>
      *************************                                         <PHLRMS>
      *                                                                 <PHLRMS>
       4210-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     TV058-TV058-REC.              <PHLRMS>
           MOVE TV058                  TO WSAA-TABLE.                   <PHLRMS>
           MOVE SV029-NEWSTS           TO WSAA-ITEM.                    <PHLRMS>
           PERFORM 1900-READ-TABLE.                                     <PHLRMS>
           MOVE ITEM-GENAREA           TO TV058-TV058-REC.              <PHLRMS>
      *                                                                 <PHLRMS>
       4290-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       4300-CHECK-PRINT SECTION.                                        <PHLRMS>
      **************************                                        <PHLRMS>
      *                                                                 <PHLRMS>
       4310-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE 'N'                    TO  WSAA-FLAG-PRINT.             <PHLRMS>
           IF  SV029-RSNUPD            NOT = SPACES                     <PHLRMS>
               PERFORM VARYING IDX FROM 1  BY 1 UNTIL IDX > 20          <PHLRMS>
                                       OR  WSAA-FLAG-PRINT   = 'Y'      <PHLRMS>
                                       OR  TV058-RSCDE (IDX) = SPACES   <PHLRMS>
                   IF  TV058-RSCDE(IDX) = SV029-RSNUPD                  <PHLRMS>
                   AND TV058-PRTOPT(IDX) = 'Y'                          <PHLRMS>
                       MOVE 'Y'        TO  WSAA-FLAG-PRINT              <PHLRMS>
                   END-IF                                               <PHLRMS>
               END-PERFORM                                              <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  WSAA-FLAG-PRINT         = 'Y'                            <PHLRMS>
               ADD 1                   TO WSAA-COUNT-PRINT              <PHLRMS>
               PERFORM 4400-KEEP-INFO-PRINT                             <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       4390-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <PHLRMS>
       4400-KEEP-INFO-PRINT SECTION.                                    <PHLRMS>
      ******************************                                    <PHLRMS>
      *                                                                 <PHLRMS>
       4410-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Insert record for printing                                   <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     RETR-PARAMS.                  <PHLRMS>
           MOVE WSAA-SEQNUM            TO RETR-SEQNUM.                  <PHLRMS>
           MOVE SV029-ENTYNUM          TO RETR-ENTYNUM.                 <PHLRMS>
           MOVE SV029-RCNUM            TO RETR-RCNUM.                   <PHLRMS>
           MOVE SV029-CLTNAME          TO RETR-TXTLINE.                 <PHLRMS>
           IF  SV029-RSNUPD            NOT = SPACES                     <PHLRMS>
               MOVE TV036              TO WSAA-TABLE                    <PHLRMS>
               MOVE SV029-RSNUPD       TO WSAA-ITEM                     <PHLRMS>
               PERFORM 1800-READ-DESC                                   <PHLRMS>
               MOVE DESC-LONGDESC      TO RETR-BLRSON                   <PHLRMS>
           END-IF.                                                      <PHLRMS>
           MOVE WSAA-TODAY             TO RETR-DATREP.                  <PHLRMS>
           MOVE RETRREC                TO RETR-FORMAT.                  <PHLRMS>
           MOVE WRITR                  TO RETR-FUNCTION.                <PHLRMS>
                                                                        <PHLRMS>
           CALL 'RETRIO'               USING RETR-PARAMS.               <PHLRMS>
                                                                        <PHLRMS>
           IF  RETR-STATUZ             NOT = O-K                        <PHLRMS>
               MOVE RETR-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               MOVE RETR-PARAMS        TO SYSR-PARAMS                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       4490-EXIT.                                                       <PHLRMS>
           EXIT.                                                        <PHLRMS>
      /                                                                 <CS006>
       X100-POPUP-PDF-FILE SECTION.                                     <CS006>
      *****************************                                     <CS006>
       X101-START.                                                      <CS006>
      *                                                                 <CS006>
           PERFORM X110-READ-TV098-TABLE.                               <CS006>
                                                                        <CS006>
           IF WSAA-POPUP-PDF           = 'Y'                            <CS006>
               PERFORM X120-VIEW-PDF-FILE                               <CS006>
           END-IF.                                                      <CS006>
      *                                                                 <CS006>
       X109-EXIT.                                                       <CS006>
           EXIT.                                                        <CS006>
      /                                                                 <CS006>
       X110-READ-TV098-TABLE SECTION.                                   <CS006>
      *******************************                                   <CS006>
       X111-START.                                                      <CS006>
      *                                                                 <CS006>
           MOVE 'N'                    TO WSAA-POPUP-PDF.               <CS006>
                                                                        <CS006>
           INITIALIZE                  ITEM-PARAMS.                     <CS006>
           MOVE 'IT'                   TO ITEM-ITEMPFX.                 <CS006>
           MOVE WSAA-COMPANY           TO ITEM-ITEMCOY.                 <CS006>
           MOVE TV098                  TO ITEM-ITEMTABL.                <CS006>
           MOVE LETRQST-LETTER-TYPE    TO ITEM-ITEMITEM.                <CS006>
           MOVE SPACES                 TO ITEM-ITEMSEQ.                 <CS006>
           MOVE ITEMREC                TO ITEM-FORMAT.                  <CS006>
           MOVE READR                  TO ITEM-FUNCTION.                <CS006>
                                                                        <CS006>
           CALL 'ITEMIO'               USING ITEM-PARAMS.               <CS006>
                                                                        <CS006>
           IF ITEM-STATUZ              NOT = O-K                        <CS006>
           AND                         NOT = MRNF                       <CS006>
               MOVE ITEM-STATUZ        TO SYSR-STATUZ                   <CS006>
               MOVE ITEM-PARAMS        TO SYSR-PARAMS                   <CS006>
               PERFORM 600-FATAL-ERROR                                  <CS006>
           END-IF.                                                      <CS006>
                                                                        <CS006>
           IF ITEM-STATUZ              = O-K                            <CS006>
               MOVE ITEM-GENAREA       TO TV098-TV098-REC               <CS006>
               IF TV098-YNFLAG         = 'Y'                            <CS006>
                  MOVE 'Y'             TO WSAA-POPUP-PDF                <CS006>
               END-IF                                                   <CS006>
           END-IF.                                                      <CS006>
      *                                                                 <CS006>
       X119-EXIT.                                                       <CS006>
           EXIT.                                                        <CS006>
      /                                                                 <CS006>
       X120-VIEW-PDF-FILE  SECTION.                                     <CS006>
      *****************************                                     <CS006>
       X121-START.                                                      <CS006>
      *                                                                 <CS006>
           MOVE 'R'                    TO WSAA-MODE.                    <CS006>
           MOVE LETRQST-LETC-RRN       TO WSAA-LETC-RRN.                <CS006>
           MOVE RETRV                  TO WSAA-FUNCTION.                <CS006>
           MOVE 'E'                    TO WSAA-LANGUAGE.                <CS006>
           MOVE O-K                    TO WSAA-STATUZ.                  <CS006>
                                                                        <CS006>
           CALL 'XML400'            USING WSAA-FUNCTION                 <CS006>
                                          WSAA-MODE                     <CS006>
                                          WSAA-LETC-RRN                 <CS006>
                                          WSAA-LANGUAGE                 <CS006>
                                          WSAA-EMAIL                    <CS006>
                                          WSAA-ESUBJECT                 <CS006>
                                          WSAA-ETEXT                    <CS006>
                                          WSAA-STATUZ.                  <CS006>
                                                                        <CS006>
      *                                                                 <CS006>
       X129-EXIT.                                                       <CS006>
           EXIT.                                                        <CS006>
      /                                                                 <CS006>
