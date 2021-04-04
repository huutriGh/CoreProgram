       IDENTIFICATION DIVISION.
       PROGRAM-ID.                                       PCPLCHDR.
      * Description: (2-3 lines)
      *
      * This program is using to get the current CHDR info.
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      * IF THERE IS ANY CHANGE ON PCPLCHDRP, PLEASE CHANGE THIS PROGRAM
      * ACCORDINGLY.
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      *
      * Offset. Description...............................................
      *
      *   01    Contract Number.
      *   02    Contract Original Start Date
      *   03    Current from Date
      *   04    Current to Date
      *   05    APL Suppress From Date
      *   06    APL Suppress To Date
      *   07    Bill Suppress From Date
      *   08    Bill Suppress To Date
      *   09    Suppress Notice From Date
      *   10    Suppress Notice To Date
      *   11    BTDATE
      *   12    Bill Chnl
      *   13    Payment Description                                     <PCPPRT>
      *   14    PTDATE
      *   15    Renewal Year
      *   16    Billing Freq
      *   17    Billing Freq Description
      *   18    Contract type
      *   19    Contract type Description
      *   20    Status Code
      *   21    Status Description
      *   22    SINSTAMT06
      *   23    Rnwl supr from
      *   24    Rnwl supr to
      *   25    Branch Number
      *   26    Branch Name
      *   27    Contract Currency
      *   28    Contract Currency Description
      *   29    Agent Number
      *   30    Agent Name
      *   31    Owner Number
      *   32    Owner Name
      *   33    Owner ID
      *   34    Agent Area Code
      *   35    Agent Sales Unit
      *   36    Projected Paid-To-Date
      *   37    Health Certificate Date
      *   38    Next anniversary date.
      *   41    HI Risk Cess Date.
      *
      *  Overview.
      *  ---------
      *
      *  Processing.
      *  -----------
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
      * 13/01/97  01/01   PCPRT        Andrew Morando                       *
      *                            ORIGINAL PROGRAM.                        *
      *                                                                     *
      * 25/10/99  01/01   LGI001       Kakit                                *
      *           Modify to check for key change after BEGN CHDRIO     ??   *
      *                                                                     *
      * 30/08/01  01/01   PCPPRT       Tan Yoke Wah                         *
      *          -To include APL Suppress From Date, APL Suppress To Date,  *
      *           Bill Suppress From Date, Bill Suppress To Date,           *
      *           Suppress Notice From Date, Suppress Notice To Date        *
      *                                                                     *
      * 03/10/01  01/01   PCPPRT       Saw Geok Tin                         *
      *          -To include Payment Description & Renewal Year             *
      *                                                                     *
      * 23/10/01  01/01   PCPPRT       Wang Ge                              *
      *          -Use VRCM-MAX-DATE, not LETC-LETTER-REQUEST-DATE to        *
      *           CHDR records, otherwise, the result may be wrong.         *
      *          -To include more fields
      *                                                                     *
      * 23/08/13  01/01   NB002        FOR CSC WORKING 1                    *
      *           TO GET CONTRACT OWNER ID                                  *
      *                                                                     *
      * 11/09/13  01/01   NB009        Sang Nguyen - CSC Developer          *
      *           VIETNAMESE FORMAT NAME                                    *
      *                                                                     *
      * 01/10/13  01/01   PS003        Khang Nguyen - CSC Developer         *
      *           Add new field of Agent Sales Unit and Area Code           *
      *                                                                     *
      * 17/07/14  01/01   PHE002       Phuong Le Dev                        *
      *           Get Projected Paid-To-Date                                *
      *                                                                     *
      * 18/07/14  01/01   PHE003       Phuong Le Dev                        *
      *           Get Health Check Date                                     *
      *                                                                     *
      * 18/07/14  01/01   GAPPH2       Tuan Le                              *
      *           Get PTDATE field.                                         *
      *                                                                     *
      * 09/11/16  01/01   PHFX11       Phi Tran - IT DEV                    *
      *           Get Correct PTDATE for Laps Reinstatement Letter.         *
      *                                                                     *
      * 27/11/17  01/01   LET03        Phi Tran - IT DEV                    *
      *           Add Field:                                                *
      *            - Sale Unit Name
      *            - DM Name
      *                                                                     *
      * 29/01/19  01/01   PS012        Tuyet Huynh IT - DEV                 *
      *           Add field: Next anniversary date.                         *
      *                                                                     *
      * 27/05/19  01/01   PS012        Vo Thi Hong Duyen - IT               *
      *           Add field Anniversary date which use for HI risk          *
      *           cess date.                                                *
      *                                                                     *
      **DD/MM/YY*************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WSAA-SUBRNAME               PIC X(10) VALUE 'PCPLCHDR  '.
       01  WSAA-LAST-IDCODE-COUNT      PIC S9(18) COMP-3 VALUE ZERO.
       01  WSAA-DATE                   PIC X(10).
       01  WSAA-STORE-RRN              PIC S9(09) COMP-3.
       01  WSAA-AGENT-FOUND            PIC X(01).                       <LET03>
       01  WSAA-REPORTAG               PIC X(08).                       <LET03>

      *  Make this field as large as you think the the data which
      *  will be stored in it is ever likely to be. Calcualate
      *  carefully as too small will result in truncation and too
      *  large will result in poor performance.

       01  WSAA-DATA-BUFFER            PIC X(1024) VALUE SPACES.

       01  WSAA-INSTTO                 PIC 9(08).                       <PCPPRT>
       01  FILLER  REDEFINES WSAA-INSTTO.                               <PCPPRT>
           03 WSAA-INSTO-YR            PIC 9(04).                       <PCPPRT>
           03 WSAA-INSTO-MM            PIC 9(02).                       <PCPPRT>
           03 WSAA-INSTO-DD            PIC 9(02).                       <PCPPRT>

       01  WSAA-SINSTAMT06             PIC ZZ,ZZZ,ZZZ,ZZ9.99.
       01  WSAA-INST-OUTST             PIC 9(02).                       <PHE002>
       01  WSAA-PRJ-PTD                PIC S9(08).                      <PHE002>
       01  WSAA-HC-DATE                PIC S9(08).                      <PHE003>
                                                                        <PS012>
       01  WSAA-NEXT-ANNIVER           PIC X(10).                       <PS012>
       01  WSAA-ANNIVER-DATE           PIC X(10).                       <PS012>
       01  WSAA-POLICY-YEAR            PIC S9(03).                      <PS012>
       01  WSAA-FREQ-FACTOR            PIC S9(06)V9(05).                <PS012>
      *
      * The following array is configured to store up to 20 fields,
      * ensure you change it to store exactly how many fields are
      * stored/retrieved by this subroutine.

       01  OFFSET                      PIC S9(03) COMP-3.
       01  FILLER.
      **** 03  WSAA-START-AND-LENGTH   OCCURS 37.               <LET03> <PHE003>
      *    03  WSAA-START-AND-LENGTH   OCCURS 39.               <PS012> <LET03>
      *    03  WSAA-START-AND-LENGTH   OCCURS 40.                       <PS012>
           03  WSAA-START-AND-LENGTH   OCCURS 41.                       <PS012>
               05  STRPOS              PIC S9(05) COMP-3.
               05  FLDLEN              PIC S9(05) COMP-3.

      *01  FILLER.
      *    03  WSAA-START-AND-LENGTH   OCCURS 32.
      *        05  STRPOS              PIC S9(05) COMP-3.
      *        05  FLDLEN              PIC S9(05) COMP-3.

       01  FORMATS.
           03  CHDRREC                 PIC X(10) VALUE 'CHDRREC'.
           03  AGNTREC                 PIC X(10) VALUE 'AGNTREC'.
           03  AGLFREC                 PIC X(10) VALUE 'AGLFREC'.       <PS003>
           03  ITEMREC                 PIC X(10) VALUE 'ITEMREC'.
           03  PAYRREC                 PIC X(10) VALUE 'PAYRREC'.       <PHE002>
           03  REISREC                 PIC X(10) VALUE 'REISREC'.       <PHE003>

       01  TABLES.
           03  T1692                   PIC X(05) VALUE 'T1692'.
           03  T3620                   PIC X(05) VALUE 'T3620'.         <PCPPRT>
           03  T3590                   PIC X(05) VALUE 'T3590'.
           03  T3623                   PIC X(05) VALUE 'T3623'.
           03  T3629                   PIC X(05) VALUE 'T3629'.
           03  T5688                   PIC X(05) VALUE 'T5688'.
           03  TH558                   PIC X(05) VALUE 'TH558'.
           03  TT518                   PIC X(05) VALUE 'TT518'.         <LET03>

           COPY DATCON1REC.
           COPY DATCON2REC.                                             <PHE002>
           COPY DATCON3REC.                                             <PHE002>
           COPY DESCSKM.
           COPY SMTPFXCPY.
           COPY NAMADRSCPY.
           COPY NAMADRSREC.
           COPY SYSERRREC.
           COPY VARCOM.
           COPY CHDRSKM.
           COPY CLNTSKM.
           COPY AGNTSKM.
           COPY AGLFSKM.                                                <PS003>
           COPY TH558REC.
           COPY ITEMSKM.
           COPY PAYRSKM.                                                <PHE002>
           COPY REISSKM.                                                <PHE003>

       LINKAGE SECTION.

           COPY PCPDATAREC.
           COPY LETCSKM.

       PROCEDURE DIVISION USING PCPD-PCPDATA-REC LETC-PARAMS.
       000-MAIN SECTION.
       010-MAIN.

      *    If the LETC-RRN is different from the one we had
      *    before (or we are on the first call) we need to set up the
      *    data buffer by reading data from the database.

           IF  LETC-RRN  NOT = WSAA-STORE-RRN
               PERFORM 100-INITIALISE
               PERFORM 200-GET-CHDR-AND-FORMAT
           END-IF.

      *    Now we simply set the offset and return the data from
      *    the data buffer.

           MOVE PCPD-FLD-OFFSET        TO OFFSET.
           MOVE WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET))
                                       TO PCPD-DATA
           MOVE FLDLEN(OFFSET)         TO PCPD-DATA-LEN.
           MOVE O-K                    TO PCPD-STATUZ.

       090-EXIT.
           EXIT PROGRAM.

       100-INITIALISE SECTION.
       101-PARA.

           MOVE PCPD-IDCODE-COUNT      TO WSAA-LAST-IDCODE-COUNT.
           MOVE LETC-RRN               TO WSAA-STORE-RRN.

       109-EXIT.
           EXIT.

       200-GET-CHDR-AND-FORMAT SECTION.
       200-GET-DATA.

           MOVE 'CH'                    TO CHDR-CHDRPFX.
           MOVE LETC-RDOCCOY            TO CHDR-CHDRCOY.
           MOVE LETC-RDOCNUM            TO CHDR-CHDRNUM.
           MOVE '1'                     TO CHDR-VALIDFLAG.
      *    MOVE LETC-LETTER-REQUEST-DATE
           MOVE VRCM-MAX-DATE
                                        TO CHDR-CURRFROM.
           MOVE BEGN                    TO CHDR-FUNCTION.

           CALL 'CHDRIO' USING CHDR-PARAMS.
           IF CHDR-STATUZ        NOT = O-K
               MOVE CHDR-PARAMS      TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR.
           IF CHDR-STATUZ            = O-K AND                          <LGI001>
               (CHDR-CHDRPFX     NOT = 'CH'         OR                  <LGI001>
               CHDR-CHDRCOY      NOT = LETC-RDOCCOY OR                  <LGI001>
               CHDR-CHDRNUM      NOT = LETC-RDOCNUM)                    <LGI001>
               MOVE CHDR-PARAMS     TO SYSR-PARAMS                      <LGI001>
               PERFORM 600-FATAL-ERROR                                  <LGI001>
           END-IF.                                                      <LGI001>

       201-FORMAT-CHDRNUM.
           MOVE 1 TO OFFSET

           MOVE 1                        TO STRPOS(OFFSET).
           MOVE LENGTH OF CHDR-CHDRNUM TO FLDLEN(OFFSET).
           MOVE CHDR-CHDRNUM
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       202-FORMAT-ORIG-COMM-DATE.
           ADD 1  TO OFFSET

           MOVE CHDR-OCCDATE        TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       203-FORMAT-EFFECTIVE-DATE.
           ADD 1  TO OFFSET

           MOVE CHDR-CURRFROM       TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       204-FORMAT-EFFECTIVE-TO.
           ADD 1  TO OFFSET

           MOVE CHDR-CURRTO            TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       205-FORMAT-APLSPFROM-DATE.
           ADD 1  TO OFFSET

           MOVE CHDR-APLSPFROM         TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       206-FORMAT-APLSPTO-DATE.
           ADD 1  TO OFFSET

           MOVE CHDR-APLSPTO           TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       207-FORMAT-BILLSPFROM-DATE.
           ADD 1  TO OFFSET

           MOVE CHDR-BILLSPFROM        TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       208-FORMAT-BILLSPTO-DATE.
           ADD 1  TO OFFSET

           MOVE CHDR-BILLSPTO          TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       209-FORMAT-NOTSSPFROM-DATE.
           ADD 1  TO OFFSET

           MOVE CHDR-NOTSSPFROM        TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       210-FORMAT-NOTSSPTO-DATE.
           ADD 1  TO OFFSET

           MOVE CHDR-NOTSSPTO          TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       211-FORMAT-BTDATE.
           ADD 1  TO OFFSET                                             <PCPPRT>
                                                                        <PCPPRT>
      *    MOVE CHDR-BILLCD            TO DTC1-INT-DATE.                <PCPPRT>
           MOVE CHDR-BTDATE            TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.                <PCPPRT>
           CALL 'DATCON1' USING DTC1-DATCON1-REC.                       <PCPPRT>
                                                                        <PCPPRT>
           COMPUTE STRPOS(OFFSET) =                                     <PCPPRT>
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).                 <PCPPRT>
           MOVE 10                     TO FLDLEN(OFFSET).               <PCPPRT>
           MOVE DTC1-EXT-DATE                                           <PCPPRT>
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).        <PCPPRT>
      *
       212-FORMAT-BILLCHNL.
           ADD 1 TO OFFSET.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE LENGTH OF CHDR-BILLCHNL  TO FLDLEN(OFFSET).
           MOVE CHDR-BILLCHNL
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       213-FORMAT-PAYMENT-DESC.                                         <PCPPRT>
           ADD 1  TO OFFSET                                             <PCPPRT>
                                                                        <PCPPRT>
           MOVE SPACES                 TO DESC-PARAMS.                  <PCPPRT>
           MOVE T3620                  TO DESC-DESCTABL.                <PCPPRT>
           MOVE CHDR-BILLCHNL          TO DESC-DESCITEM.                <PCPPRT>
           PERFORM 510-CALL-DESC.
                                                                        <PCPPRT>
           COMPUTE STRPOS(OFFSET) =                                     <PCPPRT>
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).                 <PCPPRT>
           MOVE 30                     TO FLDLEN(OFFSET).               <PCPPRT>
           MOVE DESC-LONGDESC                                           <PCPPRT>
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).        <PCPPRT>

       214-FORMAT-PTDATE.
           ADD 1  TO OFFSET.

      **** MOVE CHDR-BTDATE            TO DTC1-INT-DATE.                <GAPPH2>
           MOVE CHDR-PTDATE            TO DTC1-INT-DATE.                <GAPPH2>
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       215-FORMAT-RENEWAL-YEAR.                                         <PCPPRT>
           ADD 1  TO OFFSET                                             <PCPPRT>
                                                                        <PCPPRT>
      *    MOVE CHDRLNB-PTDATE         TO WSAA-INSTTO.                  <PCPPRT>
           MOVE CHDR-PTDATE            TO WSAA-INSTTO.
                                                                        <PCPPRT>
           COMPUTE STRPOS(OFFSET) =                                     <PCPPRT>
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).                 <PCPPRT>
           MOVE LENGTH OF WSAA-INSTO-YR TO FLDLEN(OFFSET).              <PCPPRT>
           MOVE WSAA-INSTO-YR                                           <PCPPRT>
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).        <PCPPRT>
      *
       216-FORMAT-BILLFREQ.
           ADD 1  TO OFFSET

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE LENGTH OF CHDR-BILLFREQ TO FLDLEN(OFFSET).
           MOVE CHDR-BILLFREQ
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       217-FORMAT-BILLFREQ-DESC.
           ADD 1  TO OFFSET

           MOVE SPACES                 TO DESC-PARAMS.
           MOVE PCPD-LANGUAGE          TO DESC-LANGUAGE.
           MOVE T3590                  TO DESC-DESCTABL.
           MOVE CHDR-BILLFREQ          TO DESC-DESCITEM.
           PERFORM 510-CALL-DESC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 30                     TO FLDLEN(OFFSET).
           MOVE DESC-LONGDESC
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       218-FORMAT-CNTTYPE.
           ADD 1  TO OFFSET

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE LENGTH OF CHDR-CNTTYPE TO FLDLEN(OFFSET).
           MOVE CHDR-CNTTYPE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       219-FORMAT-CNTTYPE-DESC.
           ADD 1  TO OFFSET
           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).

      *
      * Read TH558 for extendend description for contract type, if not
      * found, read T5688.
      *
           MOVE SPACES                 TO ITEM-DATA-KEY.
           MOVE O-K                    TO ITEM-STATUZ.
           MOVE SMTP-ITEM              TO ITEM-ITEMPFX.
           MOVE CHDR-CHDRCOY           TO ITEM-ITEMCOY.
           MOVE TH558                  TO ITEM-ITEMTABL.
           STRING PCPD-LANGUAGE      DELIMITED BY SIZE
                  CHDR-CNTTYPE       DELIMITED BY SIZE
             INTO ITEM-ITEMITEM.
           MOVE ITEMREC                TO ITEM-FORMAT.
           MOVE READR                  TO ITEM-FUNCTION.

           CALL 'ITEMIO' USING ITEM-PARAMS.

           IF  ITEM-STATUZ             NOT = O-K
           AND ITEM-STATUZ             NOT = MRNF
               MOVE ITEM-STATUZ        TO SYSR-STATUZ
               MOVE ITEM-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           IF ITEM-STATUZ              = O-K
              MOVE ITEM-GENAREA        TO TH558-TH558-REC
              IF  TH558-ADSC       NOT = SPACES
                  MOVE LENGTH OF TH558-ADSC
                                       TO FLDLEN(OFFSET)
                  MOVE TH558-ADSC
                  TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET))
              END-IF
           ELSE
              MOVE SPACES              TO DESC-PARAMS
              MOVE T5688               TO DESC-DESCTABL
              MOVE CHDR-CNTTYPE        TO DESC-DESCITEM
              PERFORM 510-CALL-DESC

              MOVE 30                  TO FLDLEN(OFFSET)
              MOVE DESC-LONGDESC
                TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET))
           END-IF.
      *
       220-FORMAT-STATCODE.
           ADD 1  TO OFFSET

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE LENGTH OF CHDR-STATCODE TO FLDLEN(OFFSET).
           MOVE CHDR-STATCODE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       221-FORMAT-STATCODE-DESC.
           ADD 1  TO OFFSET

           MOVE SPACES                 TO DESC-PARAMS.
           MOVE T3623                  TO DESC-DESCTABL.
           MOVE CHDR-STATCODE          TO DESC-DESCITEM.
           PERFORM 510-CALL-DESC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 30                     TO FLDLEN(OFFSET).
           MOVE DESC-LONGDESC
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       222-FORMAT-SINSTAMT06.
           ADD 1  TO OFFSET

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).

           MOVE CHDR-SINSTAMT06        TO WSAA-SINSTAMT06.
           MOVE LENGTH OF WSAA-SINSTAMT06
                                       TO FLDLEN(OFFSET).

           MOVE WSAA-SINSTAMT06
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       223-FORMAT-RNWLSPFROM.
           ADD 1  TO OFFSET

           MOVE CHDR-RNWLSPFROM        TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       224-FORMAT-RNWLSPTO.
           ADD 1  TO OFFSET

           MOVE CHDR-RNWLSPTO          TO DTC1-INT-DATE.
           MOVE CONV                   TO DTC1-FUNCTION.
           CALL 'DATCON1' USING DTC1-DATCON1-REC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 10                     TO FLDLEN(OFFSET).
           MOVE DTC1-EXT-DATE
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       225-FORMAT-CNTBRANCH.
           ADD 1  TO OFFSET

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE LENGTH OF CHDR-CNTBRANCH
                                       TO FLDLEN(OFFSET).
           MOVE CHDR-CNTBRANCH
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       226-FORMAT-CNTBRANCH-DESC.
           ADD 1  TO OFFSET

           MOVE SPACES                 TO DESC-PARAMS.
           MOVE T1692                  TO DESC-DESCTABL.
           MOVE CHDR-CNTBRANCH         TO DESC-DESCITEM.
           PERFORM 510-CALL-DESC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 30                     TO FLDLEN(OFFSET).
           MOVE DESC-LONGDESC
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       227-FORMAT-CNTCURR.
           ADD 1  TO OFFSET

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE LENGTH OF CHDR-CNTCURR TO FLDLEN(OFFSET).
           MOVE CHDR-CNTCURR
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *
       228-FORMAT-CNTCURR-DESC.
           ADD 1  TO OFFSET

           MOVE SPACES                 TO DESC-PARAMS.
           MOVE T3629                  TO DESC-DESCTABL.
           MOVE CHDR-CNTCURR           TO DESC-DESCITEM.
           PERFORM 510-CALL-DESC.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE 30                     TO FLDLEN(OFFSET).
           MOVE DESC-LONGDESC
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       229-FORMAT-AGNTNUM.
           ADD 1  TO OFFSET.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE LENGTH OF CHDR-AGNTNUM TO FLDLEN(OFFSET).

           MOVE CHDR-AGNTNUM
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *

       229-FORMAT-AGNTNAME.
           PERFORM 520-GET-AGENT-NAME.

           ADD 1                       TO OFFSET.
           COMPUTE STRPOS(OFFSET)      =
                   STRPOS(OFFSET - 1)  + FLDLEN(OFFSET - 1).

           MOVE ZERO                   TO FLDLEN(OFFSET)
           INSPECT NMAD-NAME       TALLYING FLDLEN(OFFSET)
               FOR CHARACTERS BEFORE INITIAL '    '.
           IF FLDLEN(OFFSET)           = ZERO
              MOVE 1                   TO FLDLEN(OFFSET)
           END-IF.

           MOVE NMAD-NAME(1:FLDLEN(OFFSET))
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       229-FORMAT-COWNNUM.
           ADD 1  TO OFFSET.

           COMPUTE STRPOS(OFFSET) =
               STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE LENGTH OF CHDR-COWNNUM TO FLDLEN(OFFSET).

           MOVE CHDR-COWNNUM
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
      *

       229-FORMAT-OWNER-NAME.
           ADD 1                       TO OFFSET.
           COMPUTE STRPOS(OFFSET)      =
                   STRPOS(OFFSET - 1)  + FLDLEN(OFFSET - 1).

           MOVE SPACES                 TO NMAD-NAMADRS-REC.
           MOVE 'CN'                   TO NMAD-CLNT-PREFIX.
           MOVE CHDR-COWNCOY           TO NMAD-CLNT-COMPANY.
           MOVE CHDR-COWNNUM           TO NMAD-CLNT-NUMBER.
           MOVE PCPD-LANGUAGE          TO NMAD-LANGUAGE.
      *    MOVE 'PYNMN'                TO NMAD-FUNCTION.                <NB009>
           MOVE 'LGNMF'                TO NMAD-FUNCTION.                <NB009>
           CALL 'NAMADRS'           USING NMAD-NAMADRS-REC.

           IF NMAD-STATUZ          NOT  = O-K
              MOVE NMAD-STATUZ         TO SYSR-STATUZ
              MOVE NMAD-NAMADRS-REC    TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.


           MOVE ZERO                   TO FLDLEN(OFFSET)
           INSPECT NMAD-NAME       TALLYING FLDLEN(OFFSET)
               FOR CHARACTERS BEFORE INITIAL '    '.
           IF FLDLEN(OFFSET)           = ZERO
              MOVE 1                   TO FLDLEN(OFFSET)
           END-IF.

           MOVE NMAD-NAME(1:FLDLEN(OFFSET))
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       299-FORMAT-OWNER-ID.

           MOVE CHDR-COWNPFX           TO CLNT-CLNTPFX.
           MOVE CHDR-COWNCOY           TO CLNT-CLNTCOY.
           MOVE CHDR-COWNNUM           TO CLNT-CLNTNUM.
           MOVE READR                  TO CLNT-FUNCTION.

           CALL 'CLNTIO' USING CLNT-PARAMS.

           IF  CLNT-STATUZ          NOT = O-K
              MOVE CLNT-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.
           ADD 1                       TO  OFFSET.
           COMPUTE STRPOS(OFFSET)   =
             STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).

           MOVE LENGTH OF CLNT-SECUITYNO
                                       TO FLDLEN(OFFSET).
           MOVE CLNT-SECUITYNO
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).
                                                                        <PS003>
       300-CALL-AGLF.                                                   <PS003>
                                                                        <PS003>
           MOVE SPACES              TO AGLF-DATA-AREA.                  <PS003>
           MOVE CHDR-AGNTCOY        TO AGLF-AGNTCOY.                    <PS003>
           MOVE CHDR-AGNTNUM        TO AGLF-AGNTNUM.                    <PS003>
           MOVE AGLFREC             TO AGLF-FORMAT.                     <PS003>
           MOVE READR               TO AGLF-FUNCTION.                   <PS003>
                                                                        <PS003>
           CALL 'AGLFIO'            USING AGLF-PARAMS.                  <PS003>
                                                                        <PS003>
           IF  CLNT-STATUZ          NOT = O-K                           <PS003>
              MOVE CLNT-PARAMS         TO SYSR-PARAMS                   <PS003>
              PERFORM 600-FATAL-ERROR                                   <PS003>
           END-IF.                                                      <PS003>
                                                                        <PS003>
       301-FORMAT-AREA-CODE.                                            <PS003>
                                                                        <PS003>
           ADD 1                       TO  OFFSET.                      <PS003>
           COMPUTE STRPOS(OFFSET)   =                                   <PS003>
             STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).                   <PS003>
                                                                        <PS003>
           MOVE LENGTH OF AGLF-ARACDE                                   <PS003>
                                       TO FLDLEN(OFFSET).               <PS003>
           MOVE AGLF-ARACDE                                             <PS003>
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).        <PS003>

       302-FORMAT-SALES-UNIT.                                           <PS003>
                                                                        <PS003>
           ADD 1                       TO  OFFSET.                      <PS003>
           COMPUTE STRPOS(OFFSET)   =                                   <PS003>
             STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).                   <PS003>
                                                                        <PS003>
           MOVE LENGTH OF AGLF-TSALESUNT                                <PS003>
                                       TO FLDLEN(OFFSET).               <PS003>
           MOVE AGLF-TSALESUNT                                          <PS003>
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).        <PS003>
                                                                        <PHE002>
       303-FORMAT-PRJ-PTD.                                              <PHE002>
                                                                        <PHE002>
           MOVE 99999999               TO WSAA-PRJ-PTD.                 <PHFX11>
           ADD 1                       TO  OFFSET.                      <PHE002>
PHI   **** PERFORM 400-GET-PROJECT-PTD.                         <PHFX11><PHE002>
PHI        PERFORM 410-GET-HC-DATE                                      <PHFX11>
                                                                        <PHE002>
           IF  WSAA-PRJ-PTD            NOT = 99999999                   <PHE002>
               MOVE WSAA-PRJ-PTD       TO DTC1-INT-DATE                 <PHE002>
               MOVE CONV               TO DTC1-FUNCTION                 <PHE002>
               CALL 'DATCON1'          USING DTC1-DATCON1-REC           <PHE002>
           END-IF.                                                      <PHE002>
                                                                        <PHE002>
           COMPUTE STRPOS(OFFSET)      =                                <PHE002>
             STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).                   <PHE002>
                                                                        <PHE002>
           IF  WSAA-PRJ-PTD            = 99999999                       <PHE002>
               MOVE 1                  TO FLDLEN(OFFSET)                <PHE002>
               MOVE SPACES                                              <PHE002>
               TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET))       <PHE002>
           ELSE                                                         <PHE002>
               MOVE 10                 TO FLDLEN(OFFSET)                <PHE002>
               MOVE DTC1-EXT-DATE                                       <PHE002>
               TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET))       <PHE002>
           END-IF.                                                      <PHE002>
                                                                        <PHE003>
       304-FORMAT-HC-DATE.                                              <PHE003>
                                                                        <PHE003>
           ADD 1                       TO  OFFSET.                      <PHE003>
PHI   **** PERFORM 410-GET-HC-DATE.                             <PHFX11><PHE003>
                                                                        <PHE003>
           IF  WSAA-HC-DATE            NOT = 99999999                   <PHE003>
               MOVE WSAA-HC-DATE       TO DTC1-INT-DATE                 <PHE003>
               MOVE CONV               TO DTC1-FUNCTION                 <PHE003>
               CALL 'DATCON1'          USING DTC1-DATCON1-REC           <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           COMPUTE STRPOS(OFFSET)      =                                <PHE003>
             STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).                   <PHE003>
                                                                        <PHE003>
           IF  WSAA-HC-DATE            = 99999999                       <PHE003>
               MOVE 1                  TO FLDLEN(OFFSET)                <PHE003>
               MOVE SPACES                                              <PHE003>
               TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET))       <PHE003>
           ELSE                                                         <PHE003>
               MOVE 10                 TO FLDLEN(OFFSET)                <PHE003>
               MOVE DTC1-EXT-DATE                                       <PHE003>
               TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET))       <PHE003>
           END-IF.                                                      <PHE003>
      *                                                                 <LET03>
       338-FORMAT-SALE-UNIT-NAME.                                       <LET03>
           ADD 1                       TO OFFSET.                       <LET03>
           COMPUTE STRPOS(OFFSET)      =                                <LET03>
                   STRPOS(OFFSET - 1)  + FLDLEN(OFFSET - 1).            <LET03>
                                                                        <LET03>
           INITIALIZE                     DESC-PARAMS.                  <LET03>
           MOVE TT518                  TO DESC-DESCTABL.                <LET03>
           MOVE AGLF-TSALESUNT         TO DESC-DESCITEM.                <LET03>
           MOVE 'IT'                   TO DESC-DESCPFX.                 <LET03>
           MOVE CHDR-CHDRCOY           TO DESC-DESCCOY.                 <LET03>
           MOVE PCPD-LANGUAGE          TO DESC-LANGUAGE.                <LET03>
           MOVE READR                  TO DESC-FUNCTION.                <LET03>
                                                                        <LET03>
           CALL 'DESCIO'            USING DESC-PARAMS.                  <LET03>
                                                                        <LET03>
           IF DESC-STATUZ           NOT = O-K AND MRNF                  <LET03>
              MOVE DESC-STATUZ         TO SYSR-STATUZ                   <LET03>
              MOVE DESC-PARAMS         TO SYSR-PARAMS                   <LET03>
           END-IF.                                                      <LET03>
                                                                        <LET03>
           MOVE LENGTH OF DESC-LONGDESC                                 <LET03>
                                       TO FLDLEN(OFFSET).               <LET03>
           MOVE DESC-LONGDESC                                           <LET03>
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).        <LET03>
      *                                                                 <LET03>
       339-FORMAT-AGENT-DM.                                             <LET03>
           ADD 1                       TO OFFSET.                       <LET03>
           COMPUTE STRPOS(OFFSET)      =                                <LET03>
                   STRPOS(OFFSET - 1)  + FLDLEN(OFFSET - 1).            <LET03>
                                                                        <LET03>
           MOVE 'N'                    TO WSAA-AGENT-FOUND.             <LET03>
           MOVE SPACES                 TO NMAD-NAMADRS-REC.             <LET03>
           MOVE AGLF-REPORTAG          TO WSAA-REPORTAG.                <LET03>
           PERFORM A1000-GET-DM-AGENT UNTIL WSAA-AGENT-FOUND = 'Y'      <LET03>
                                      OR WSAA-REPORTAG = SPACES.        <LET03>
                                                                        <LET03>
           MOVE ZERO                   TO FLDLEN(OFFSET).               <LET03>
           INSPECT NMAD-NAME       TALLYING FLDLEN(OFFSET)              <LET03>
               FOR CHARACTERS BEFORE INITIAL '    '.                    <LET03>
           IF FLDLEN(OFFSET)           = ZERO                           <LET03>
              MOVE 1                   TO FLDLEN(OFFSET)                <LET03>
           END-IF.                                                      <LET03>
                                                                        <LET03>
           MOVE NMAD-NAME(1:FLDLEN(OFFSET))                             <LET03>
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).        <LET03>
      *                                                                 <PS012>
       340-NEXT-ANNIVERSARY-DATE.                                       <PS012>
           ADD 1                       TO OFFSET.                       <PS012>
           COMPUTE STRPOS(OFFSET)      =                                <PS012>
                   STRPOS(OFFSET - 1)  + FLDLEN(OFFSET - 1).            <PS012>
                                                                        <PS012>
      ***  PERFORM 700-NEXT-ANNIVERSARY-DATE.                           <PS012>
                                                                        <PS012>
      ***  MOVE LENGTH OF WSAA-NEXT-ANNIVER                             <PS012>
      ***                              TO FLDLEN(OFFSET).               <PS012>
      ***                                                               <PS012>
      ***  MOVE WSAA-NEXT-ANNIVER      TO                               <PS012>
      ***       WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).        <PS012>
                                                                        <PS012>
           PERFORM 800-ANNIVERSARY-DATE.                                <PS012>
                                                                        <PS012>
           MOVE LENGTH OF WSAA-ANNIVER-DATE                             <PS012>
                                       TO FLDLEN(OFFSET).               <PS012>
                                                                        <PS012>
           MOVE WSAA-ANNIVER-DATE      TO                               <PS012>
                WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).        <PS012>
      *                                                                 <PS012>
       341-HI-RISK-CESS-DATE.                                           <PS012>
      *                                                                 <PS012>
           ADD 1                       TO OFFSET.                       <PS012>
           COMPUTE STRPOS(OFFSET)      =                                <PS012>
                   STRPOS(OFFSET - 1)  + FLDLEN(OFFSET - 1).            <PS012>
                                                                        <PS012>
      **** PERFORM 800-ANNIVERSARY-DATE.                                <PS012>
                                                                        <PS012>
           MOVE LENGTH OF WSAA-ANNIVER-DATE                             <PS012>
                                       TO FLDLEN(OFFSET).               <PS012>
                                                                        <PS012>
           MOVE WSAA-ANNIVER-DATE      TO                               <PS012>
                WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).        <PS012>
      *                                                                 <LET03>
       299-EXIT.                                                        <PHE003>
           EXIT.                                                        <PHE003>
      /                                                                 <PHE002>
       400-GET-PROJECT-PTD SECTION.                                     <PHE002>
      *****************************                                     <PHE002>
      *                                                                 <PHE002>
       401-START.                                                       <PHE003>
      *                                                                 <PHE002>
           MOVE 99999999               TO WSAA-PRJ-PTD.                 <PHE002>
      *                                                                 <PHE002>
      *--  Get Project PTD in case Lapse Re-Instatement         <PHE003><PHE002>
      *                                                                 <PHE002>
           IF  LETC-LETTER-TYPE        NOT = 'HRSTLET'                  <PHE002>
               GO TO 409-EXIT                                           <PHE003>
           END-IF.                                                      <PHE002>
                                                                        <PHE002>
           INITIALIZE                     PAYR-PARAMS.                  <PHE002>
           MOVE LETC-RDOCCOY           TO PAYR-CHDRCOY.                 <PHE002>
           MOVE LETC-RDOCNUM           TO PAYR-CHDRNUM.                 <PHE002>
           MOVE 1                      TO PAYR-PAYRSEQNO.               <PHE002>
           MOVE PAYRREC                TO PAYR-FORMAT.                  <PHE002>
           MOVE READR                  TO PAYR-FUNCTION.                <PHE002>
                                                                        <PHE002>
           CALL 'PAYRIO'               USING PAYR-PARAMS.               <PHE002>
                                                                        <PHE002>
           IF  PAYR-STATUZ             NOT = O-K                        <PHE002>
           AND                         NOT = MRNF                       <PHE002>
               MOVE PAYR-STATUZ        TO SYSR-STATUZ                   <PHE002>
               MOVE PAYR-PARAMS        TO SYSR-PARAMS                   <PHE002>
               PERFORM 600-FATAL-ERROR                                  <PHE002>
           END-IF.                                                      <PHE002>
                                                                        <PHE002>
           IF  PAYR-STATUZ             = MRNF                           <PHE002>
               GO TO 409-EXIT                                           <PHE003>
           END-IF.                                                      <PHE002>
                                                                        <PHE002>
           MOVE SPACES                 TO DTC3-DATCON3-REC.             <PHE002>
           MOVE PAYR-BILLCD            TO DTC3-INT-DATE-1.              <PHE002>
           MOVE LETC-OTHER-KEYS(3:8)   TO DTC3-INT-DATE-2.              <PHE002>
           MOVE PAYR-BILLFREQ          TO DTC3-FREQUENCY.               <PHE002>
                                                                        <PHE002>
           CALL 'DATCON3'              USING DTC3-DATCON3-REC.          <PHE002>
                                                                        <PHE002>
           IF  DTC3-STATUZ             NOT = '****'                     <PHE002>
               MOVE DTC3-DATCON3-REC   TO SYSR-PARAMS                   <PHE002>
               PERFORM 600-FATAL-ERROR                                  <PHE002>
           END-IF.                                                      <PHE002>
                                                                        <PHE002>
           MOVE DTC3-FREQ-FACTOR       TO WSAA-INST-OUTST.              <PHE002>
           ADD  1                      TO WSAA-INST-OUTST.              <PHE002>
                                                                        <PHE002>
           MOVE SPACES                 TO DTC2-DATCON2-REC.             <PHE002>
           MOVE PAYR-PTDATE            TO DTC2-INT-DATE-1.              <PHE002>
           MOVE WSAA-INST-OUTST        TO DTC2-FREQ-FACTOR.             <PHE002>
           MOVE PAYR-BILLFREQ          TO DTC2-FREQUENCY.               <PHE002>
                                                                        <PHE002>
           CALL 'DATCON2'              USING DTC2-DATCON2-REC.          <PHE002>
                                                                        <PHE002>
           IF  DTC2-STATUZ             NOT = O-K                        <PHE002>
               MOVE DTC2-DATCON2-REC   TO SYSR-PARAMS                   <PHE002>
               PERFORM 600-FATAL-ERROR                                  <PHE002>
           END-IF.                                                      <PHE002>
                                                                        <PHE002>
           MOVE DTC2-INT-DATE-2        TO WSAA-PRJ-PTD.                 <PHE002>
      *                                                                 <PHE002>
       409-EXIT.                                                        <PHE003>
           EXIT.                                                        <PHE003>
      /                                                                 <PHE003>
       410-GET-HC-DATE SECTION.                                         <PHE003>
      *************************                                         <PHE003>
      *                                                                 <PHE003>
       411-START.                                                       <PHE003>
      *                                                                 <PHE003>
           MOVE 99999999               TO WSAA-HC-DATE.                 <PHE003>
                                                                        <PHE003>
      *--  Get HC Date in case Lapse Re-Instatement                     <PHE003>
                                                                        <PHE003>
           IF  LETC-LETTER-TYPE        NOT = 'HRSTLET'                  <PHE003>
               GO TO 419-EXIT                                           <PHE003>
           END-IF.                                                      <PHE003>
      *                                                                 <PHE003>
      *--  Get Newest Record Lapse Re-Instatement                       <PHE003>
      *                                                                 <PHE003>
           INITIALIZE                     REIS-PARAMS.                  <PHE003>
           MOVE LETC-RDOCNUM           TO REIS-CHDRNUM.                 <PHE003>
           MOVE REISREC                TO REIS-FORMAT.                  <PHE003>
           MOVE READR                  TO REIS-FUNCTION.                <PHE003>
                                                                        <PHE003>
           CALL 'REISIO'               USING REIS-PARAMS.               <PHE003>
                                                                        <PHE003>
           IF  REIS-STATUZ             NOT = O-K                        <PHE003>
           AND                         NOT = MRNF                       <PHE003>
               MOVE REIS-STATUZ        TO SYSR-STATUZ                   <PHE003>
               MOVE REIS-PARAMS        TO SYSR-PARAMS                   <PHE003>
               PERFORM 600-FATAL-ERROR                                  <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           IF  REIS-STATUZ             = MRNF                           <PHE003>
               GO TO 419-EXIT                                           <PHE003>
           END-IF.                                                      <PHE003>
                                                                        <PHE003>
           MOVE REIS-ACCPTDTE          TO WSAA-HC-DATE.                 <PHE003>
PHI        MOVE REIS-HPRJPTDATE        TO WSAA-PRJ-PTD.                 <PHFX11>
      *                                                                 <PHE003>
       419-EXIT.                                                        <PHE003>
      *    EXIT.                                                        <PHE003>

       500-CALL-DATCON1 SECTION.
       501-START.

           MOVE CONV                   TO DTC1-FUNCTION.

           CALL 'DATCON1'              USING DTC1-DATCON1-REC.

           IF  DTC1-STATUZ NOT = O-K
               MOVE DTC1-DATCON1-REC   TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR
           END-IF.

           MOVE DTC1-EXT-DATE          TO WSAA-DATE.

       509-EXIT.
           EXIT.
      *
       510-CALL-DESC SECTION.
       510-START.
           MOVE SMTP-ITEM              TO DESC-DESCPFX.
           MOVE CHDR-CHDRCOY           TO DESC-DESCCOY.
           MOVE PCPD-LANGUAGE          TO DESC-LANGUAGE.
           MOVE READR                  TO DESC-FUNCTION.
      *
           CALL 'DESCIO'            USING DESC-PARAMS.
           IF DESC-STATUZ           NOT = O-K
              MOVE SPACES              TO DESC-LONGDESC
           END-IF.
      *
       519-EXIT.
           EXIT.

       520-GET-AGENT-NAME SECTION.
      *****************************
       520-AGENT-NAME.
           MOVE CHDR-AGNTPFX           TO AGNT-AGNTPFX.
           MOVE CHDR-AGNTCOY           TO AGNT-AGNTCOY.
           MOVE CHDR-AGNTNUM           TO AGNT-AGNTNUM.
           MOVE READR                  TO AGNT-FUNCTION.
           MOVE AGNTREC                TO AGNT-FORMAT.
           CALL 'AGNTIO'            USING AGNT-PARAMS.

           IF AGNT-STATUZ          NOT  = O-K
              MOVE AGNT-STATUZ         TO SYSR-STATUZ
              MOVE AGNT-PARAMS         TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

      *
           MOVE AGNT-CLNTPFX           TO NMAD-CLNT-PREFIX.
           MOVE AGNT-CLNTCOY           TO NMAD-CLNT-COMPANY.
           MOVE AGNT-CLNTNUM           TO NMAD-CLNT-NUMBER.
           MOVE PCPD-LANGUAGE          TO NMAD-LANGUAGE.
      *    MOVE 'PYNMN'                TO NMAD-FUNCTION.                <NB009>
           MOVE 'LGNMF'                TO NMAD-FUNCTION.                <NB009>
           CALL 'NAMADRS'           USING NMAD-NAMADRS-REC.

           IF NMAD-STATUZ          NOT  = O-K
              MOVE NMAD-STATUZ         TO SYSR-STATUZ
              MOVE NMAD-NAMADRS-REC    TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR
           END-IF.

      *
       529-EXIT.
           EXIT.

       600-FATAL-ERROR SECTION.
       610-FATAL.
           MOVE WSAA-SUBRNAME          TO SYSR-SUBRNAME.
           MOVE BOMB                   TO PCPD-STATUZ.
           IF  SYSR-STATUZ             NOT = SPACES
           OR  SYSR-SYSERR-STATUZ      NOT = SPACES
              MOVE '1'                 TO SYSR-SYSERR-TYPE
           ELSE
              MOVE '2'                 TO SYSR-SYSERR-TYPE.
           CALL 'SYSERR'               USING SYSR-SYSERR-REC.

       690-EXIT.
           EXIT PROGRAM.
      /                                                                 <LET03>
       700-NEXT-ANNIVERSARY-DATE SECTION.                               <PS012>
      ************************************                              <PS012>
       701-START.                                                       <PS012>
      *                                                                 <PS012>
           MOVE SPACES                 TO WSAA-NEXT-ANNIVER.            <PS012>
           MOVE ZEROES                 TO WSAA-POLICY-YEAR.             <PS012>
           MOVE ZEROES                 TO WSAA-FREQ-FACTOR.             <PS012>
                                                                        <PS012>
           MOVE SPACES                 TO DTC3-DATCON3-REC.             <PS012>
           MOVE CHDR-OCCDATE           TO DTC3-INT-DATE-1.              <PS012>
           MOVE LETC-LETTER-REQUEST-DATE TO DTC3-INT-DATE-2.            <PS012>
           MOVE '01'                   TO DTC3-FREQUENCY.               <PS012>
           MOVE ZERO                   TO DTC3-FREQ-FACTOR.             <PS012>
                                                                        <PS012>
           CALL 'DATCON3'              USING DTC3-DATCON3-REC.          <PS012>
                                                                        <PS012>
           IF DTC3-STATUZ              NOT = O-K                        <PS012>
               MOVE DTC3-DATCON3-REC   TO SYSR-PARAMS                   <PS012>
               MOVE DTC3-STATUZ        TO SYSR-STATUZ                   <PS012>
               PERFORM 600-FATAL-ERROR                                  <PS012>
           END-IF.                                                      <PS012>
                                                                        <PS012>
           MOVE DTC3-FREQ-FACTOR       TO WSAA-POLICY-YEAR.             <PS012>
           MOVE DTC3-FREQ-FACTOR       TO WSAA-FREQ-FACTOR.             <PS012>
           ADD 1                       TO WSAA-POLICY-YEAR.             <PS012>
                                                                        <PS012>
           INITIALIZE                  DTC2-DATCON2-REC.                <PS012>
           MOVE '01'                   TO DTC2-FREQUENCY.               <PS012>
           MOVE CHDR-OCCDATE           TO DTC2-INT-DATE-1.              <PS012>
           MOVE WSAA-POLICY-YEAR       TO DTC2-FREQ-FACTOR.             <PS012>
           CALL 'DATCON2'              USING DTC2-DATCON2-REC.          <PS012>
                                                                        <PS012>
           IF DTC2-STATUZ              NOT = O-K                        <PS012>
               MOVE DTC2-DATCON2-REC   TO SYSR-PARAMS                   <PS012>
               MOVE DTC2-STATUZ        TO SYSR-STATUZ                   <PS012>
               PERFORM 600-FATAL-ERROR                                  <PS012>
           END-IF.                                                      <PS012>
                                                                        <PS012>
           MOVE SPACES                 TO DTC1-DATCON1-REC.             <PS012>
           MOVE DTC2-INT-DATE-2        TO DTC1-INT-DATE.                <PS012>
           MOVE CONV                   TO DTC1-FUNCTION                 <PS012>
                                                                        <PS012>
           CALL 'DATCON1'              USING DTC1-DATCON1-REC.          <PS012>
                                                                        <PS012>
           IF DTC1-STATUZ              NOT = O-K                        <PS012>
               MOVE DTC1-DATCON1-REC   TO SYSR-PARAMS                   <PS012>
               MOVE DTC1-STATUZ        TO SYSR-STATUZ                   <PS012>
               PERFORM 600-FATAL-ERROR                                  <PS012>
           END-IF.                                                      <PS012>
                                                                        <PS012>
           MOVE DTC1-EXT-DATE          TO WSAA-NEXT-ANNIVER.            <PS012>
      *                                                                 <PS012>
       702-EXIT.                                                        <PS012>
           EXIT.                                                        <PS012>
      /                                                                 <PS012>
       800-ANNIVERSARY-DATE SECTION.                                    <PS012>
      ******************************                                    <PS012>
       701-START.                                                       <PS012>
      *                                                                 <PS012>
           MOVE SPACES                 TO WSAA-ANNIVER-DATE.            <PS012>
           MOVE ZEROES                 TO WSAA-POLICY-YEAR.             <PS012>
      *                                                                 <PS012>
           IF  CHDR-PTDATE              = ZEROES                        <PS012>
           OR  CHDR-PTDATE              =  99999999                     <PS012>
               GO TO 890-EXIT                                           <PS012>
           END-IF.                                                      <PS012>
                                                                        <PS012>
           MOVE SPACES                 TO DTC3-DATCON3-REC.             <PS012>
           MOVE CHDR-OCCDATE           TO DTC3-INT-DATE-1.              <PS012>
           MOVE CHDR-PTDATE            TO DTC3-INT-DATE-2.              <PS012>
           MOVE '01'                   TO DTC3-FREQUENCY.               <PS012>
           MOVE ZERO                   TO DTC3-FREQ-FACTOR.             <PS012>
      *                                                                 <PS012>
           CALL 'DATCON3'           USING DTC3-DATCON3-REC.             <PS012>
      *                                                                 <PS012>
           IF  DTC3-STATUZ          NOT = O-K                           <PS012>
               MOVE DTC3-DATCON3-REC   TO SYSR-PARAMS                   <PS012>
               MOVE DTC3-STATUZ        TO SYSR-STATUZ                   <PS012>
               PERFORM 600-FATAL-ERROR                                  <PS012>
           END-IF.                                                      <PS012>
      *                                                                 <PS012>
           IF  DTC3-FREQ-FACTOR        >= 3                             <PS012>
           AND CHDR-PTDATE              < LETC-LETTER-REQUEST-DATE      <PS012>
               MOVE WSAA-FREQ-FACTOR   TO DTC3-FREQ-FACTOR              <PS012>
           END-IF.                                                      <PS012>
                                                                        <PS012>
           ADD 0.99999                 TO DTC3-FREQ-FACTOR.             <PS012>
           MOVE DTC3-FREQ-FACTOR       TO WSAA-POLICY-YEAR.             <PS012>
                                                                        <PS012>
           INITIALIZE                  DTC2-DATCON2-REC.                <PS012>
           MOVE '01'                   TO DTC2-FREQUENCY.               <PS012>
           MOVE CHDR-OCCDATE           TO DTC2-INT-DATE-1.              <PS012>
           MOVE WSAA-POLICY-YEAR       TO DTC2-FREQ-FACTOR.             <PS012>
      *                                                                 <PS012>
           CALL 'DATCON2'           USING DTC2-DATCON2-REC.             <PS012>
      *                                                                 <PS012>
           IF  DTC2-STATUZ          NOT = O-K                           <PS012>
               MOVE DTC2-DATCON2-REC   TO SYSR-PARAMS                   <PS012>
               MOVE DTC2-STATUZ        TO SYSR-STATUZ                   <PS012>
               PERFORM 600-FATAL-ERROR                                  <PS012>
           END-IF.                                                      <PS012>
                                                                        <PS012>
           MOVE SPACES                 TO DTC1-DATCON1-REC.             <PS012>
           MOVE DTC2-INT-DATE-2        TO DTC1-INT-DATE.                <PS012>
           MOVE CONV                   TO DTC1-FUNCTION                 <PS012>
      *                                                                 <PS012>
           CALL 'DATCON1'           USING DTC1-DATCON1-REC.             <PS012>
      *                                                                 <PS012>
           IF  DTC1-STATUZ          NOT = O-K                           <PS012>
               MOVE DTC1-DATCON1-REC   TO SYSR-PARAMS                   <PS012>
               MOVE DTC1-STATUZ        TO SYSR-STATUZ                   <PS012>
               PERFORM 600-FATAL-ERROR                                  <PS012>
           END-IF.                                                      <PS012>
                                                                        <PS012>
           MOVE DTC1-EXT-DATE          TO WSAA-ANNIVER-DATE.            <PS012>
      *                                                                 <PS012>
       890-EXIT.                                                        <PS012>
           EXIT.                                                        <PS012>
      /                                                                 <PS012>
       A1000-GET-DM-AGENT SECTION.                                      <LET03>
      ****************************                                      <LET03>
       A1100-START.                                                     <LET03>
      *                                                                 <LET03>
           MOVE WSAA-REPORTAG          TO AGLF-AGNTNUM.                 <LET03>
           MOVE READR                  TO AGLF-FUNCTION.                <LET03>
           CALL 'AGLFIO'            USING AGLF-PARAMS.                  <LET03>
                                                                        <LET03>
           IF AGLF-STATUZ           NOT = O-K AND MRNF                  <LET03>
              MOVE AGLF-STATUZ         TO SYSR-STATUZ                   <LET03>
              MOVE AGLF-PARAMS         TO SYSR-PARAMS                   <LET03>
              PERFORM 600-FATAL-ERROR                                   <LET03>
           END-IF.                                                      <LET03>
                                                                        <LET03>
           IF AGLF-STATUZ               = O-K                           <LET03>
              INITIALIZE                  AGNT-PARAMS                   <LET03>
              MOVE CHDR-AGNTPFX        TO AGNT-AGNTPFX                  <LET03>
              MOVE CHDR-AGNTCOY        TO AGNT-AGNTCOY                  <LET03>
              MOVE AGLF-AGNTNUM        TO AGNT-AGNTNUM                  <LET03>
              MOVE READR               TO AGNT-FUNCTION                 <LET03>
              MOVE AGNTREC             TO AGNT-FORMAT                   <LET03>
              CALL 'AGNTIO'         USING AGNT-PARAMS                   <LET03>
              IF AGNT-STATUZ          NOT  = O-K                        <LET03>
                 MOVE AGNT-STATUZ         TO SYSR-STATUZ                <LET03>
                 MOVE AGNT-PARAMS         TO SYSR-PARAMS                <LET03>
                 PERFORM 600-FATAL-ERROR                                <LET03>
              END-IF                                                    <LET03>
              IF AGNT-AGTYPE            = 'DM'                          <LET03>
                 MOVE 'Y'              TO WSAA-AGENT-FOUND              <LET03>
                 MOVE AGNT-CLNTPFX     TO NMAD-CLNT-PREFIX              <LET03>
                 MOVE AGNT-CLNTCOY     TO NMAD-CLNT-COMPANY             <LET03>
                 MOVE AGNT-CLNTNUM     TO NMAD-CLNT-NUMBER              <LET03>
                 MOVE PCPD-LANGUAGE    TO NMAD-LANGUAGE                 <LET03>
                 MOVE 'PYNMN'          TO NMAD-FUNCTION                 <LET03>
                 MOVE 'LGNMF'          TO NMAD-FUNCTION                 <LET03>
                 CALL 'NAMADRS'     USING NMAD-NAMADRS-REC              <LET03>
                                                                        <LET03>
                 IF NMAD-STATUZ    NOT  = O-K                           <LET03>
                    MOVE NMAD-STATUZ   TO SYSR-STATUZ                   <LET03>
                    MOVE NMAD-NAMADRS-REC TO SYSR-PARAMS                <LET03>
                    PERFORM 600-FATAL-ERROR                             <LET03>
                 END-IF                                                 <LET03>
              END-IF                                                    <LET03>
           END-IF.                                                      <LET03>
                                                                        <LET03>
           MOVE AGLF-REPORTAG          TO WSAA-REPORTAG.                <LET03>
      *                                                                 <LET03>
       A1900-EXIT.                                                      <LET03>
            EXIT.                                                       <LET03>
      /                                                                 <LET03>

