      * Generation Parameters - SCRVER(02)            Do Not Delete|
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PV032.
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
      * 12/02/14  01/01   PHLRMS       Phuong Le Dev                        *
      *           Receipt Information                                       *
      *                                                                     *
      * 25/10/17  01/01   CS002        Tuyet Huynh IT - DEV                 *
      *           Re-Assign Date.                                           *
      *                                                                     *
      * 02/04/18  01/01   CS007        Tuyet Huynh IT - DEV                 *
      *           Add Receiver on behalf ,Guarantee person field.           *
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
       01  WSAA-PROG                   PIC X(05) VALUE 'PV032'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
       01  ERRORS.
           03  E005                    PIC X(04) VALUE 'E005'.
           03  F136                    PIC X(04) VALUE 'F136'.
           03  G437                    PIC X(04) VALUE 'G437'.
           03  H093                    PIC X(04) VALUE 'H093'.          <PHLRMS>
      *
       01  TABLES.
           03  TV020                   PIC X(05) VALUE 'TV020'.
           03  TV022                   PIC X(05) VALUE 'TV022'.
           03  TV041                   PIC X(05) VALUE 'TV041'.
           03  TV047                   PIC X(05) VALUE 'TV047'.
           03  TV036                   PIC X(05) VALUE 'TV036'.
           03  TV057                   PIC X(05) VALUE 'TV057'.         <CS007>
      *
       01  FORMATS.
           03  TRRNCDEREC              PIC X(10) VALUE 'TRRNCDEREC'.    <PHLRMS>
           03  TRRNSTSREC              PIC X(10) VALUE 'TRRNSTSREC'.
           03  CLTSREC                 PIC X(10) VALUE 'CLTSREC'.
           03  AGNTREC                 PIC X(10) VALUE 'AGNTREC'.
           03  DESCREC                 PIC X(10) VALUE 'DESCREC'.
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  CHDRLNBREC              PIC X(10) VALUE 'CHDRLNBREC'.
           03  RCPYACTREC              PIC X(10) VALUE 'RCPYACTREC'.
           03  RDOCREC                 PIC X(10) VALUE 'RDOCREC'.
           03  RCOLREC                 PIC X(10) VALUE 'RCOLREC'.
           03  RCNTREC                 PIC X(10) VALUE 'RCNTREC'.       <CS007>
      /
       01  WSAA-CONFIRM-MESSAGE        PIC X(15) VALUE
           'Print Receipt :'.

       01  WSAA-ASGNDATE               PIC S9(08).                      <PHLRMS>
       01  WSAA-X                      PIC S9(03) COMP-3 VALUE 0.       <PHLRMS>
       01  WSAA-Y                      PIC S9(03) COMP-3 VALUE 0.       <PHLRMS>
       01  WSAA-SUBF-RRN               PIC S9(02) VALUE ZEROES.         <PHLRMS>
       01  WSAA-CASHNM                 PIC X(47).
       01  WSAA-CLNT-FUNCTION          PIC X(04) VALUE 'CLNT'.
       01  WSAA-PAYEE-GIVN             PIC X(05) VALUE 'PYNMN'.
       01  WSAA-LP                     PIC X(02) VALUE 'LP'.
       01  WSAA-S                      PIC X(02) VALUE 'S '.
       01  WSAA-EXIST-RCPY             PIC X(01).
       01  WSAA-TODAY                  PIC S9(08).
       01  WSAA-TABLE                  PIC X(05).
       01  WSAA-ITEM                   PIC X(10).
       01  WSAA-CHDRNUM                PIC X(08).
       01  WSAA-PFX                    PIC X(02).
       01  WSAA-COY                    PIC X(01).
       01  WSAA-NUM                    PIC X(08).
       01  WSAA-UPDATE                 PIC X(01).
       01  WSAA-ENTYNUM                PIC X(08).
       01  WSAA-DESC                   PIC X(47).
       01  WSAA-PREFIX                 PIC X(02).
       01  WSAA-TYPE                   PIC X(10).
       01  WSAA-TRANCDE                PIC X(04).                       <CS002>
       01  FILLER.                                                      <PHLRMS>
           03  WSAA-SEC-PROG           PIC X(05) OCCURS 8.              <PHLRMS>
      /
           COPY VARCOM.
           COPY GENSSWREC.                                              <PHLRMS>
           COPY SMTPFXCPY.
           COPY FSUPFXCPY.
           COPY CASHTYPCPY.
           COPY CSHRFCPY.

           COPY SYSERRREC.
           COPY OPSTATSREC.
           COPY DATCON1REC.
           COPY GETCLNTREC.
           COPY NAMADRSREC.
      /
       01  WSAA-BATCKEY.
           COPY BATCKEY.
      *
           COPY TV036REC.
           COPY TV047REC.
           COPY DESCSKM.
           COPY ITEMSKM.
           COPY TRRNCDESKM.                                             <PHLRMS>
           COPY TRRNSTSSKM.
           COPY CLTSSKM.
           COPY AGNTSKM.
           COPY RCOLSKM.
           COPY RDOCSKM.
           COPY CHDRLNBSKM.
           COPY RCPYACTSKM.
           COPY RCNTSKM.                                                <CS007>
      /
       LINKAGE SECTION.

           COPY WSSPCOMN.
      /
       01  WSSP-USER-AREA              PIC X(768).
      /
           COPY SCRNPARAMS.
      /
           COPY SV032SCR.
      /
       PROCEDURE DIVISION USING WSSP-COMMON-AREA WSSP-USER-AREA         <PHLRMS>
                                SCRN-SCREEN-PARAMS SV032-DATA-AREA.

           COPY MAING.
           COPY CONFNAME.
      /
      *****************************************************************
      *      INITIALISE FIELDS FOR SHOWING ON SCREEN
      *****************************************************************
      *
       1000-INITIALISE SECTION.
      *************************
      *
       1010-INITIALISE.
      *                                                                 <PHLRMS>
      *-- Skip this section if returning from an optional selection     <PHLRMS>
      *                                                                 <PHLRMS>
           IF  WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                   <PHLRMS>
               GO TO 1090-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>

           MOVE WSSP-BATCHKEY          TO WSAA-BATCKEY.
           MOVE SPACES                 TO SV032-DATA-AREA.

      *    Dummy field initilisation for prototype version.
******     MOVE ZERO
******          TO SV032-AMNT                     .
******     MOVE MAXDATE
******          TO SV032-DUEDTE                   .
******     MOVE MAXDATE
******          TO SV032-EXPDTE                   .
******     MOVE SPACES
******          TO SV032-PURPYMT                  .
******     MOVE SPACES
******          TO SV032-CHDRNUM                  .
******     MOVE SPACES
******          TO SV032-COWNNUM                  .
******     MOVE SPACES
******          TO SV032-CLNTNAME                 .
******     MOVE SPACES
******          TO SV032-RECEIPT                  .
******     MOVE SPACES
******          TO SV032-RFNUM                    .
******     MOVE SPACES
******          TO SV032-CASHNM                   .
******     MOVE SPACES
******          TO SV032-LOCALITY                 .
******     MOVE SPACES
******          TO SV032-CLTNAMES                 .
******     MOVE SPACES                                                  <PHLRMS>
******          TO SV032-SELECT                   .                     <PHLRMS>
******     MOVE SPACES                                                  <PHLRMS>
******          TO SV032-DOCTDESC                 .                     <PHLRMS>
******
           MOVE SPACES                 TO SV032-SPCAPP                  <CS007>
                                          SV032-SPCAPPD                 <CS007>
                                          SV032-RCVCODE                 <CS007>
                                          SV032-RCVNAME                 <CS007>
                                          SV032-NOTELINE.               <CS007>
                                                                        <CS007>
      *
      *--  Set screen fields
      *
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

           MOVE RETRV                  TO TRRNSTS-FUNCTION.

           CALL 'TRRNSTSIO'            USING TRRNSTS-PARAMS.

           IF  TRRNSTS-STATUZ          NOT = O-K
               MOVE TRRNSTS-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE TRRNSTS-RCNUM          TO SV032-RCNUM.
      *
      *--  From location
      *
           MOVE TRRNSTS-FRMPFX         TO WSAA-PREFIX.
           MOVE SPACES                 TO WSAA-DESC.
           MOVE SPACES                 TO WSAA-TYPE.

           IF  TRRNSTS-FRMPFX          = 'OF'
               MOVE TRRNSTS-FROMLOC    TO SV032-ENTYNUM-01
               MOVE TRRNSTS-FROMLOC    TO WSAA-ENTYNUM
               PERFORM 2100-GET-OFF-DESC
           ELSE
               MOVE TRRNSTS-FROMPER    TO SV032-ENTYNUM-01
               MOVE TRRNSTS-FROMPER    TO WSAA-ENTYNUM
               PERFORM 2200-GET-PER-DESC
           END-IF.

           MOVE WSAA-DESC              TO SV032-CLTNAME-01.
           MOVE WSAA-TYPE              TO SV032-ADMINUSER-1.
      *
      *--  To location
      *
           MOVE TRRNSTS-TOPFX          TO WSAA-PREFIX.
           MOVE SPACES                 TO WSAA-DESC.
           MOVE SPACES                 TO WSAA-TYPE.

           IF  TRRNSTS-TOPFX           = 'OF'
               MOVE TRRNSTS-TOLOC      TO SV032-ENTYNUM-02
               MOVE TRRNSTS-TOLOC      TO WSAA-ENTYNUM
               PERFORM 2100-GET-OFF-DESC
           ELSE
               MOVE TRRNSTS-TOPER      TO SV032-ENTYNUM-02
               MOVE TRRNSTS-TOPER      TO WSAA-ENTYNUM
               PERFORM 2200-GET-PER-DESC
           END-IF.

           MOVE WSAA-DESC              TO SV032-CLTNAME-02.
           MOVE WSAA-TYPE              TO SV032-ADMINUSER-2.
      *
      *--  Print date
      *
           IF  TRRNSTS-PRINTDT         = VRCM-MAX-DATE
               MOVE 'N'                TO SV032-YNFLAG
           ELSE
               MOVE 'Y'                TO SV032-YNFLAG
           END-IF.
      *
      *--  Status Description
      *
           MOVE TV020                  TO WSAA-TABLE.
           MOVE TRRNSTS-RCSTAT         TO WSAA-ITEM.

           PERFORM 1800-GET-ITEM-DESCRIPTION.

           MOVE DESC-LONGDESC          TO SV032-RCSTATDES.
      *
      *--  Reason
      *
           MOVE SPACES                 TO TV036-TV036-REC.
           MOVE TV036                  TO WSAA-TABLE .
           MOVE TRRNSTS-RSCDE          TO WSAA-ITEM  .
           PERFORM 1900-READ-TABLE.
           MOVE ITEM-GENAREA           TO TV036-TV036-REC.
           MOVE TV036-TXTLINE          TO SV032-TXTLINE.
      *                                                                 <PHLRMS>
      *--  Remark                                                       <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE TRRNSTS-DOCTDESC       TO SV032-DOCTDESC.               <PHLRMS>
      *
      *--  Status of flow
      *
           IF  TRRNSTS-VALIDFLAG       = '3'
               MOVE 'Pending Transfer' TO SV032-STATDETS
           ELSE
               MOVE SPACES             TO SV032-STATDETS
           END-IF.
      *                                                                 <PHLRMS>
      *--  Assigned Date                                                <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE SPACES                 TO WSAA-TRANCDE.                 <CS002>
           IF TRRNSTS-RCSTAT           = 'RA'                           <CS002>
               MOVE 'TA19'             TO WSAA-TRANCDE                  <CS002>
           ELSE                                                         <CS002>
               MOVE 'TA08'             TO WSAA-TRANCDE                  <CS002>
           END-IF.                                                      <CS002>
                                                                        <CS002>
           PERFORM 2300-GET-ASSIGNED-DATE.                              <PHLRMS>
           MOVE WSAA-ASGNDATE          TO SV032-ASGNDATE.               <PHLRMS>
      *
      *--  Expiry Date
      *
           MOVE TRRNSTS-EXPDTE         TO SV032-EXPDTE.
      *
      *--  Location
      *
           MOVE TRRNSTS-LOCALITY       TO SV032-LOCALITY.
      *
      *--  Location Description
      *
           MOVE TV022                  TO WSAA-TABLE.
           MOVE TRRNSTS-LOCALITY       TO WSAA-ITEM.

           PERFORM 1800-GET-ITEM-DESCRIPTION.

           MOVE DESC-LONGDESC          TO SV032-CLTNAME-03.
      *                                                                 <CS007>
      *-- Receiver on behalf                                            <CS007>
      *                                                                 <CS007>
           IF TRRNSTS-RCVCODE          NOT = SPACES                     <CS007>
              MOVE SPACES              TO WSAA-DESC                     <CS007>
              MOVE TRRNSTS-RCVCODE     TO SV032-RCVCODE                 <CS007>
                                          WSAA-ENTYNUM                  <CS007>
              PERFORM 1400-GET-AGNT-INFO                                <CS007>
              MOVE WSAA-DESC           TO SV032-RCVNAME                 <CS007>
           END-IF.                                                      <CS007>
      *                                                                 <CS007>
      *-- Guarantee person                                              <CS007>
      *                                                                 <CS007>
           IF TRRNSTS-SPCAPP           NOT = SPACES                     <CS007>
              MOVE TRRNSTS-SPCAPP      TO SV032-SPCAPP                  <CS007>
                                          WSAA-ITEM                     <CS007>
              MOVE TV057               TO WSAA-TABLE                    <CS007>
              PERFORM 1800-GET-ITEM-DESCRIPTION                         <CS007>
              MOVE DESC-LONGDESC       TO SV032-SPCAPPD                 <CS007>
           END-IF.                                                      <CS007>
      *                                                                 <CS007>
      *-- NOTES.                                                        <CS007>
      *                                                                 <CS007>
           PERFORM 5000-LOAD-NOTE-RCNT.                                 <CS007>
                                                                        <CS007>
           IF RCNT-STATUZ              = MRNF                           <CS007>
              MOVE SPACES              TO SV032-NOTELINE                <CS007>
           ELSE                                                         <CS007>
              MOVE RCNT-NOTELINE       TO SV032-NOTELINE                <CS007>
           END-IF.                                                      <CS007>
      *
      *--  Read information for Receipt
      *
           INITIALIZE                     RCOL-PARAMS.
           MOVE TRRNSTS-RCTYPE         TO RCOL-RCTYPE.
           MOVE TRRNSTS-RCSERNUM       TO RCOL-RCSERNUM.
           MOVE RCOLREC                TO RCOL-FORMAT.
           MOVE READR                  TO RCOL-FUNCTION.

           CALL 'RCOLIO'               USING RCOL-PARAMS.

           IF  RCOL-STATUZ             NOT = O-K
           AND                         NOT = MRNF
               MOVE RCOL-PARAMS        TO SYSR-PARAMS
               MOVE RCOL-STATUZ        TO SYSR-STATUZ
               PERFORM 600-FATAL-ERROR
           END-IF.

           PERFORM 1100-READ-RCPY.

           IF  RCOL-STATUZ             = O-K
           AND RCOL-VALIDFLAG          = '1'                            <PHLRMS>
               MOVE RCOL-RDOCNUM       TO SV032-RECEIPT
               PERFORM 3200-GET-PAYOR-INFO
               MOVE RCOL-RFNUM         TO SV032-RFNUM
               MOVE WSAA-CASHNM        TO SV032-CASHNM
      *
      *--  Get information of RDOC (if any)
      *
               PERFORM 3100-READ-RDOC

               MOVE RCOL-DOCORIGAMT    TO SV032-AMNT
               IF  RCOL-CHDRNUM        NOT = SPACES
      *
      *--  Read contract header
      *
                   MOVE RCOL-CHDRNUM   TO SV032-CHDRNUM
                   MOVE RCOL-CHDRNUM   TO WSAA-CHDRNUM
                   PERFORM 1200-READ-CHDR
                   MOVE CHDRLNB-COWNNUM
                                       TO SV032-COWNNUM
      *
      *--  Get Policy Owner name
      *
                   PERFORM 1300-GET-PO
               END-IF
           ELSE
               IF  RCPYACT-STATUZ      = O-K
                   MOVE RCPYACT-CHDRNUM
                                       TO WSAA-CHDRNUM
                   PERFORM 1600-GET-MORE-INFO
               END-IF
           END-IF.
      *                                                                 <PHLRMS>
      *--  Read TV041 to get Description of Purpose Code                <PHLRMS>
      *                                                                 <PHLRMS>
           IF  RCPYACT-STATUZ          = O-K                            <PHLRMS>
               IF  RCPYACT-PURCODE     NOT = SPACES                     <PHLRMS>
                   MOVE TV041          TO WSAA-TABLE                    <PHLRMS>
                   MOVE RCPYACT-PURCODE                                 <PHLRMS>
                                       TO WSAA-ITEM                     <PHLRMS>
                   PERFORM 1800-GET-ITEM-DESCRIPTION                    <PHLRMS>
                   MOVE DESC-LONGDESC  TO SV032-PURPYMT                 <PHLRMS>
               END-IF                                                   <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Due Date                                                     <PHLRMS>
      *                                                                 <PHLRMS>
               MOVE RCPYACT-DATEDUE    TO SV032-DUEDTE                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *
       1090-EXIT.
            EXIT.
      /
       1100-READ-RCPY SECTION.
      ************************
      *
       1110-START.
      *
           INITIALIZE                     RCPYACT-PARAMS.
           MOVE TRRNSTS-RCTYPE         TO RCPYACT-RCTYPE.
           MOVE TRRNSTS-RCSERNUM       TO RCPYACT-RCSERNUM.
           MOVE RCPYACTREC             TO RCPYACT-FORMAT.
           MOVE READR                  TO RCPYACT-FUNCTION.

           CALL 'RCPYACTIO'            USING RCPYACT-PARAMS.

           IF  RCPYACT-STATUZ          NOT = O-K
           AND                         NOT = MRNF
               MOVE RCPYACT-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       1190-EXIT.
            EXIT.
      /
       1200-READ-CHDR SECTION.
      ************************
      *
       1210-START.
      *
           INITIALIZE                     CHDRLNB-PARAMS.
           MOVE WSSP-COMPANY           TO CHDRLNB-CHDRCOY.
           MOVE WSAA-CHDRNUM           TO CHDRLNB-CHDRNUM.
           MOVE CHDRLNBREC             TO CHDRLNB-FORMAT.
           MOVE READR                  TO CHDRLNB-FUNCTION.

           CALL 'CHDRLNBIO'            USING CHDRLNB-PARAMS.

           IF  CHDRLNB-STATUZ          NOT = O-K
               MOVE CHDRLNB-STATUZ     TO SYSR-STATUZ
               MOVE CHDRLNB-PARAMS     TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       1290-EXIT.
            EXIT.
      /
       1300-GET-PO SECTION.
      *********************
      *
       1310-START.
      *
           MOVE SPACES                 TO NMAD-NAMADRS-REC.
           MOVE CHDRLNB-COWNPFX        TO NMAD-CLNT-PREFIX
           MOVE CHDRLNB-COWNCOY        TO NMAD-CLNT-COMPANY
           MOVE CHDRLNB-COWNNUM        TO NMAD-CLNT-NUMBER
           MOVE WSSP-LANGUAGE          TO NMAD-LANGUAGE.
           MOVE WSAA-PAYEE-GIVN        TO NMAD-FUNCTION.

           CALL 'NAMADRS'           USING NMAD-NAMADRS-REC.

           IF NMAD-STATUZ           NOT = O-K
              MOVE NMAD-NAMADRS-REC    TO SYSR-PARAMS
              MOVE NMAD-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF NMAD-STATUZ              = MRNF
              MOVE ALL '?'             TO SV032-CLNTNAME
           ELSE
              MOVE NMAD-NAME           TO SV032-CLNTNAME
           END-IF.
      *
       1390-EXIT.
           EXIT.
      /
       1400-GET-AGNT-INFO SECTION.
      ****************************
      *
       1410-START.
      *
           PERFORM 1500-READ-AGNT.

           INITIALIZE                     CLTS-PARAMS.
           MOVE AGNT-CLNTPFX           TO WSAA-PFX   .
           MOVE AGNT-CLNTCOY           TO WSAA-COY   .
           MOVE AGNT-CLNTNUM           TO WSAA-NUM   .

           PERFORM 1700-READ-CLTS.

           MOVE WSSP-LONGCONFNAME      TO WSAA-DESC.
      *
       1490-EXIT.
            EXIT.
      /
       1500-READ-AGNT SECTION.
      ************************
      *
       1510-START.
      *
           INITIALIZE                     AGNT-PARAMS.
           MOVE 'AG'                   TO AGNT-AGNTPFX.
           MOVE WSSP-COMPANY           TO AGNT-AGNTCOY.
           MOVE WSAA-ENTYNUM           TO AGNT-AGNTNUM.
           MOVE AGNTREC                TO AGNT-FORMAT.
           MOVE READR                  TO AGNT-FUNCTION.

           CALL 'AGNTIO'               USING AGNT-PARAMS.

           IF  AGNT-STATUZ             NOT = O-K
               MOVE AGNT-STATUZ        TO SYSR-STATUZ
               MOVE AGNT-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       1590-EXIT.
            EXIT.
      /
       1600-GET-MORE-INFO SECTION.
      ****************************
      *
       1610-START.
      *
      *
      *--  Amount
      *
           MOVE RCPYACT-AMNT           TO SV032-AMNT.
      *
      *--  Get Information of Policy
      *
           IF  RCPYACT-CHDRNUM         NOT = SPACES
               MOVE RCPYACT-CHDRNUM    TO SV032-CHDRNUM
      *
      *--  Read contract header
      *
               PERFORM 1200-READ-CHDR
               MOVE CHDRLNB-COWNNUM    TO SV032-COWNNUM
      *
      *--  Get Policy Owner name
      *
               PERFORM 1300-GET-PO
           END-IF.
      *
       1690-EXIT.
            EXIT.
      /
       1700-READ-CLTS SECTION.
      ************************
      *
       1710-START.
      *
           INITIALIZE                     CLTS-PARAMS.
           MOVE WSAA-PFX               TO CLTS-CLNTPFX.
           MOVE WSAA-COY               TO CLTS-CLNTCOY.
           MOVE WSAA-NUM               TO CLTS-CLNTNUM.
           MOVE CLTSREC                TO CLTS-FORMAT.
           MOVE READR                  TO CLTS-FUNCTION.

           CALL 'CLTSIO'               USING CLTS-PARAMS.

           IF  CLTS-STATUZ             NOT = O-K
               MOVE CLTS-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *****
      *    Call the Subroutine to format the Clients Name.
      *****
           PERFORM PLAINNAME.
      *
       1790-EXIT.
            EXIT.
      /
       1800-GET-ITEM-DESCRIPTION SECTION.
      ***********************************
      *
       1810-START.
      *
           INITIALIZE                     DESC-PARAMS.
           MOVE SMTP-ITEM              TO DESC-DESCPFX.
           MOVE WSSP-COMPANY           TO DESC-DESCCOY.
           MOVE WSAA-TABLE             TO DESC-DESCTABL.
           MOVE WSAA-ITEM              TO DESC-DESCITEM.
           MOVE WSSP-LANGUAGE          TO DESC-LANGUAGE.
           MOVE 'E'                    TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
           MOVE DESCREC                TO DESC-FORMAT.

           CALL 'DESCIO'               USING DESC-PARAMS.

           IF DESC-STATUZ              NOT = O-K
           AND                         NOT = MRNF
               MOVE DESC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF DESC-STATUZ              NOT = O-K
               MOVE ALL '?'            TO DESC-SHORTDESC
                                          DESC-LONGDESC
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
           MOVE WSAA-TABLE            TO ITEM-ITEMTABL .
           MOVE WSAA-ITEM             TO ITEM-ITEMITEM.
           MOVE ITEMREC               TO ITEM-FORMAT.
           MOVE READR                 TO ITEM-FUNCTION.

           CALL  'ITEMIO'  USING ITEM-PARAMS.

           IF ITEM-STATUZ             NOT = O-K
           AND ITEM-STATUZ            NOT = MRNF
               MOVE ITEM-PARAMS       TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.
      *
       1990-EXIT.
           EXIT.
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
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <PHLRMS>
              MOVE O-K                   TO WSSP-EDTERROR               <PHLRMS>
              MOVE 3000                TO WSSP-SECTIONNO                <PHLRMS>
           END-IF.                                                      <PHLRMS>
           GO TO PRE-EXIT.                                              <PHLRMS>
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
      *--  Validate fields
      *
           IF  WSSP-FLAG               NOT = 'I'
               GO TO 2090-EXIT
           END-IF.
      *
       2080-CHECK-FOR-ERRORS.
      *
           IF SV032-ERROR-INDICATORS   NOT = SPACES
              MOVE 'Y'                 TO WSSP-EDTERROR
           END-IF.
      *
       2090-EXIT.
            EXIT.
      /
       2100-GET-OFF-DESC SECTION.
      ***************************
      *
       2110-START.
      *
           MOVE 'Office'               TO WSAA-TYPE.
      *
      *--  Office Description
      *
           MOVE TV022                  TO WSAA-TABLE.
           MOVE WSAA-ENTYNUM           TO WSAA-ITEM.

           PERFORM 1800-GET-ITEM-DESCRIPTION.

           MOVE DESC-LONGDESC          TO WSAA-DESC.
      *
       2190-EXIT.
           EXIT.
      /
       2200-GET-PER-DESC SECTION.
      ***************************
      *
       2210-START.
      *
           IF  WSAA-PREFIX             = 'AG'
               MOVE 'Agent'            TO WSAA-TYPE
      *
      *-- Get Agent Name
      *
               PERFORM 1400-GET-AGNT-INFO
           END-IF.

           IF  WSAA-PREFIX             = 'CL'
               MOVE 'Collector'        TO WSAA-TYPE
      *
      *-- Get Collector Name
      *
               MOVE TV047              TO WSAA-TABLE
               MOVE WSAA-ENTYNUM       TO WSAA-ITEM
               PERFORM 1900-READ-TABLE
               MOVE ITEM-GENAREA       TO TV047-TV047-REC
               MOVE TV047-MEMNAME      TO WSAA-DESC
           END-IF.

           IF  WSAA-PREFIX             = 'SP'
               MOVE 'Supplier'         TO WSAA-TYPE
               MOVE 'Supplier'         TO WSAA-DESC
           END-IF.
      *
       2290-EXIT.
           EXIT.
      /                                                                 <PHLRMS>
       2300-GET-ASSIGNED-DATE SECTION.                                  <PHLRMS>
      ********************************                                  <PHLRMS>
      *                                                                 <PHLRMS>
       2310-START.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
           INITIALIZE                     TRRNCDE-PARAMS.               <PHLRMS>
           MOVE TRRNSTS-RCTYPE         TO TRRNCDE-RCTYPE.               <PHLRMS>
           MOVE TRRNSTS-RCSERNUM       TO TRRNCDE-RCSERNUM.             <PHLRMS>
      *    MOVE 'TA08'                 TO TRRNCDE-TRANCDE.      <CS002> <PHLRMS>
           MOVE WSAA-TRANCDE           TO TRRNCDE-TRANCDE.              <CS002>
           MOVE TRRNCDEREC             TO TRRNCDE-FORMAT.               <PHLRMS>
           MOVE READR                  TO TRRNCDE-FUNCTION.             <PHLRMS>
                                                                        <PHLRMS>
           CALL 'TRRNCDEIO'            USING TRRNCDE-PARAMS.            <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNCDE-STATUZ          NOT = O-K                        <PHLRMS>
           AND                         NOT = MRNF                       <PHLRMS>
               MOVE TRRNCDE-PARAMS     TO SYSR-PARAMS                   <PHLRMS>
               MOVE TRRNCDE-STATUZ     TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNCDE-STATUZ          = MRNF                           <PHLRMS>
               MOVE VRCM-MAX-DATE      TO WSAA-ASGNDATE                 <PHLRMS>
               GO TO 2390-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF  TRRNCDE-STATUZ          = O-K                            <PHLRMS>
               MOVE TRRNCDE-TRANDATE   TO WSAA-ASGNDATE                 <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       2390-EXIT.                                                       <PHLRMS>
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
           IF  WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                   <PHLRMS>
               GO TO 3090-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
      *--  Update database files as required / WSSP
      *
           IF  WSSP-FLAG               NOT = 'I'
               GO TO 3090-EXIT
           END-IF.
      *
       3090-EXIT.
           EXIT.
      /
       3100-READ-RDOC SECTION.
      ************************
      *
       3110-START.
      *
           MOVE SPACES                 TO RDOC-DATA-KEY.
           MOVE PRFX-CASH              TO RDOC-RDOCPFX.
           MOVE WSSP-COMPANY           TO RDOC-RDOCCOY.
           MOVE RCOL-RDOCNUM           TO RDOC-RDOCNUM.
           MOVE '0001'                 TO RDOC-TRANSEQ.
           MOVE READR                  TO RDOC-FUNCTION.
           MOVE RDOCREC                TO RDOC-FORMAT.

           CALL 'RDOCIO'               USING RDOC-PARAMS.

           IF  RDOC-STATUZ             NOT = O-K
           AND                         NOT = MRNF
               MOVE RDOC-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF  RDOC-STATUZ             = MRNF
               GO TO 3190-EXIT
           END-IF.
      *                                                                 <PHLRMS>
      *--  Only get purpose of assigned receipt                         <PHLRMS>
      *                                                                 <PHLRMS>
      ***  MOVE RDOC-TRANDESC          TO SV032-PURPYMT.                <PHLRMS>

           IF  RDOC-SACSCODE           = WSAA-LP
           AND RDOC-SACSTYP            = WSAA-S
               MOVE RDOC-RLDGACCT      TO SV032-CHDRNUM
               MOVE RDOC-RLDGACCT      TO WSAA-CHDRNUM
               PERFORM 1200-READ-CHDR
               MOVE CHDRLNB-COWNNUM    TO SV032-COWNNUM
      *
      *--  Get Policy Owner name
      *
               PERFORM 1300-GET-PO
           END-IF.

      *
       3190-EXIT.
           EXIT.
      /
       3200-GET-PAYOR-INFO SECTION.
      *****************************
      *
       3210-START.
      *
           IF RCOL-RFCODE              = CSRF-CN
              GO TO 3220-CALL-NAMADRS
           END-IF.
      *
      *  Get the agent Client Number.
      *
           INITIALIZE                     GTCL-GETCLNT-REC.
           MOVE RCOL-RFCODE            TO GTCL-ACCT-PREFIX.
           MOVE WSSP-COMPANY           TO GTCL-ACCT-COMPANY.
           MOVE RCOL-RFNUM             TO GTCL-ACCT-NUMBER.
           MOVE WSAA-CLNT-FUNCTION     TO GTCL-FUNCTION.

           CALL 'GETCLNT'           USING GTCL-GETCLNT-REC.

           IF GTCL-STATUZ              NOT = O-K
              MOVE GTCL-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.
      *
       3220-CALL-NAMADRS.
      *
      * Format the Cash Receipt Name to Payee Name.
      *
           MOVE SPACES                 TO NMAD-NAMADRS-REC.
           IF RCOL-RFCODE              = CSRF-CN
              MOVE RCOL-RFCODE         TO NMAD-CLNT-PREFIX
              MOVE WSSP-FSUCO          TO NMAD-CLNT-COMPANY
              MOVE RCOL-RFNUM          TO NMAD-CLNT-NUMBER
           END-IF.

           IF RCOL-RFCODE              = CSRF-AG
              MOVE GTCL-CLNT-PREFIX    TO NMAD-CLNT-PREFIX
              MOVE GTCL-CLNT-COMPANY   TO NMAD-CLNT-COMPANY
              MOVE GTCL-CLNT-NUMBER    TO NMAD-CLNT-NUMBER
           END-IF.
           MOVE WSSP-LANGUAGE          TO NMAD-LANGUAGE.
           MOVE WSAA-PAYEE-GIVN        TO NMAD-FUNCTION.

           CALL 'NAMADRS'           USING NMAD-NAMADRS-REC.

           IF NMAD-STATUZ           NOT = O-K
              MOVE NMAD-NAMADRS-REC    TO SYSR-PARAMS
              MOVE NMAD-STATUZ         TO SYSR-STATUZ
              PERFORM 600-FATAL-ERROR
           END-IF.

           IF NMAD-STATUZ              = MRNF
              MOVE ALL '?'             TO WSAA-CASHNM
           ELSE
              MOVE NMAD-NAME           TO WSAA-CASHNM
           END-IF.
      *
       3290-EXIT.
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
      *
           MOVE WSAA-PROG              TO WSSP-NEXTPROG.                <PHLRMS>
                                                                        <PHLRMS>
           MOVE WSSP-COMPANY           TO GENS-COMPANY.                 <PHLRMS>
           MOVE WSAA-PROG              TO GENS-PROG-IN.                 <PHLRMS>
           MOVE WSKY-BATC-BATCTRCDE    TO GENS-TRANSACT.                <PHLRMS>
                                                                        <PHLRMS>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = SPACE                  <PHLRMS>
              MOVE WSSP-PROGRAM-PTR    TO WSAA-X                        <PHLRMS>
              MOVE 0                   TO WSAA-Y                        <PHLRMS>
              PERFORM 4100-SAVE-PROGRAM 8 TIMES                         <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF SV032-SELECT             NOT = SPACES                     <PHLRMS>
               MOVE 'I'                TO WSSP-FLAG                     <PHLRMS>
               MOVE SV032-RCNUM        TO TRRNSTS-RCNUM                 <PHLRMS>
               MOVE TRRNSTSREC         TO TRRNSTS-FORMAT                <PHLRMS>
               MOVE KEEPS              TO TRRNSTS-FUNCTION              <PHLRMS>
                                                                        <PHLRMS>
               CALL 'TRRNSTSIO'        USING TRRNSTS-PARAMS             <PHLRMS>
                                                                        <PHLRMS>
               IF  TRRNSTS-STATUZ      NOT = O-K                        <PHLRMS>
                   MOVE TRRNSTS-STATUZ TO SYSR-STATUZ                   <PHLRMS>
                   MOVE TRRNSTS-PARAMS TO SYSR-PARAMS                   <PHLRMS>
                   PERFORM 600-FATAL-ERROR                              <PHLRMS>
               END-IF                                                   <PHLRMS>
                                                                        <PHLRMS>
               MOVE 'A'                TO GENS-FUNCTION                 <PHLRMS>
               PERFORM 4300-CALL-GENSSW                                 <PHLRMS>
                                                                        <PHLRMS>
               MOVE SPACES             TO SV032-SELECT                  <PHLRMS>
               GO TO 4090-EXIT                                          <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *                                                                 <PHLRMS>
       4080-NEXT.                                                       <PHLRMS>
      *                                                                 <PHLRMS>
                                                                        <PHLRMS>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <PHLRMS>
              MOVE WSSP-PROGRAM-PTR    TO WSAA-X                        <PHLRMS>
              MOVE 0                   TO WSAA-Y                        <PHLRMS>
              PERFORM 4200-RESTORE-PROGRAM 8 TIMES                      <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
           IF WSSP-SEC-ACTN (WSSP-PROGRAM-PTR) = '*'                    <PHLRMS>
               MOVE SPACE            TO WSSP-SEC-ACTN(WSSP-PROGRAM-PTR) <PHLRMS>
               MOVE SCRN-SCRNAME     TO WSSP-NEXTPROG                   <PHLRMS>
           ELSE                                                         <PHLRMS>
               ADD 1                 TO WSSP-PROGRAM-PTR                <PHLRMS>
           END-IF.                                                      <PHLRMS>
      *
       4090-EXIT.
            EXIT.
      *                                                                 <PHLRMS>
       4100-SAVE-PROGRAM SECTION.                                       <PHLRMS>
      ***************************                                       <PHLRMS>
       4110-SAVE.                                                       <PHLRMS>
           ADD 1                       TO WSAA-X.                       <PHLRMS>
           ADD 1                       TO WSAA-Y.                       <PHLRMS>
           MOVE WSSP-SEC-PROG (WSAA-X) TO WSAA-SEC-PROG(WSAA-Y).        <PHLRMS>
      *                                                                 <PHLRMS>
       4190-EXIT.                                                       <PHLRMS>
            EXIT.                                                       <PHLRMS>
      /                                                                 <PHLRMS>
      ***************************************************************** <PHLRMS>
       4200-RESTORE-PROGRAM      SECTION.                               <PHLRMS>
      ***********************************                               <PHLRMS>
       4210-RESTORE.                                                    <PHLRMS>
           ADD 1                       TO WSAA-X.                       <PHLRMS>
           ADD 1                       TO WSAA-Y.                       <PHLRMS>
           MOVE WSAA-SEC-PROG (WSAA-Y) TO WSSP-SEC-PROG(WSAA-X).        <PHLRMS>
       4290-EXIT.                                                       <PHLRMS>
            EXIT.                                                       <PHLRMS>
                                                                        <PHLRMS>
      ***************************************************************** <PHLRMS>
       4300-CALL-GENSSW SECTION.                                        <PHLRMS>
      **************************                                        <PHLRMS>
      *                                                                 <PHLRMS>
       4310-CALL-SUBROUTINE.                                            <PHLRMS>
      *                                                                 <PHLRMS>
           CALL 'GENSSW' USING GENS-GENSSW-REC.                         <PHLRMS>
                                                                        <PHLRMS>
           IF  GENS-STATUZ             NOT = O-K                        <PHLRMS>
           AND                         NOT = MRNF                       <PHLRMS>
               MOVE GENS-STATUZ        TO SYSR-STATUZ                   <PHLRMS>
               PERFORM 600-FATAL-ERROR                                  <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
      *****                                                             <PHLRMS>
      * IF AN ENTRY ON T1675 WAS NOT FOUND BY GENSWCH REDISPLAY THE SCR <PHLRMS>
      * WITH AN ERROR AND THE OPTIONS AND EXTRAS INDICATOR              <PHLRMS>
      * WITH ITS INITIAL LOAD VALUE                                     <PHLRMS>
      *****                                                             <PHLRMS>
           IF GENS-STATUZ               = MRNF                          <PHLRMS>
              MOVE ' '                 TO WSSP-SEC-ACTN                 <PHLRMS>
                                              (WSSP-PROGRAM-PTR)        <PHLRMS>
              MOVE H093                TO SCRN-ERROR-CODE               <PHLRMS>
              MOVE SCRN-SCRNAME        TO WSSP-NEXTPROG                 <PHLRMS>
              GO TO 4390-EXIT                                           <PHLRMS>
           END-IF.                                                      <PHLRMS>
                                                                        <PHLRMS>
      *****                                                             <PHLRMS>
      *    LOAD FROM GENSW TO WSSP.                                     <PHLRMS>
      *****                                                             <PHLRMS>
                                                                        <PHLRMS>
           ADD 1, WSSP-PROGRAM-PTR GIVING WSAA-X                        <PHLRMS>
           MOVE 1                      TO WSAA-Y                        <PHLRMS>
           PERFORM 4400-LOAD-PROGRAM 8 TIMES.                           <PHLRMS>
                                                                        <PHLRMS>
           MOVE '*'                    TO                               <PHLRMS>
                                      WSSP-SEC-ACTN (WSSP-PROGRAM-PTR). <PHLRMS>
           ADD 1                       TO WSSP-PROGRAM-PTR.             <PHLRMS>
      *                                                                 <PHLRMS>
       4390-EXIT.                                                       <PHLRMS>
            EXIT.                                                       <PHLRMS>
      /                                                                 <PHLRMS>
      ***************************************************************** <PHLRMS>
       4400-LOAD-PROGRAM SECTION.                                       <PHLRMS>
      ***************************                                       <PHLRMS>
      *                                                                 <PHLRMS>
       4410-RESTORE.                                                    <PHLRMS>
      *                                                                 <PHLRMS>
           MOVE GENS-PROG-OUT (WSAA-Y) TO WSSP-SEC-PROG(WSAA-X).        <PHLRMS>
           ADD 1                       TO WSAA-X.                       <PHLRMS>
           ADD 1                       TO WSAA-Y.                       <PHLRMS>
      *                                                                 <PHLRMS>
       4490-EXIT.                                                       <PHLRMS>
            EXIT.                                                       <PHLRMS>
      /                                                                 <CS007>
       5000-LOAD-NOTE-RCNT SECTION.                                     <CS007>
      *****************************                                     <CS007>
       5001-START.                                                      <CS007>
      *                                                                 <CS007>
           INITIALIZE                  RCNT-PARAMS.                     <CS007>
           MOVE TRRNSTS-RCTYPE         TO RCNT-RCTYPE.                  <CS007>
           MOVE TRRNSTS-RCSERNUM       TO RCNT-RCSERNUM.                <CS007>
           MOVE RCNTREC                TO RCNT-FORMAT.                  <CS007>
                                                                        <CS007>
           MOVE READR                  TO RCNT-FUNCTION.                <CS007>
                                                                        <CS007>
           CALL 'RCNTIO'               USING RCNT-PARAMS.               <CS007>
                                                                        <CS007>
           IF  RCNT-STATUZ             NOT = O-K                        <CS007>
           AND                         NOT = MRNF                       <CS007>
               MOVE RCNT-STATUZ        TO SYSR-STATUZ                   <CS007>
               MOVE RCNT-PARAMS        TO SYSR-PARAMS                   <CS007>
               PERFORM 600-FATAL-ERROR                                  <CS007>
           END-IF.                                                      <CS007>
      *                                                                 <CS007>
       5009-EXIT.                                                       <CS007>
           EXIT.                                                        <CS007>
      /                                                                 <CS007>
