       IDENTIFICATION DIVISION.
       PROGRAM-ID.     ZPREMEY.
      *
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 18/06/18  01/01   NB012        Phi Tran - IT DEV                    *
      *           Initial Version.                                          *
      *           Get Plan Premium Year for UL Product.
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-AS400.
       OBJECT-COMPUTER. IBM-AS400.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WSAA-SUBRNAME               PIC X(10) VALUE 'ZPREMEY'.
       01  WSAA-LAST-IDCODE-COUNT      PIC S9(18) COMP-3 VALUE ZERO.
       01  WSAA-AMOUNT-X               PIC ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99-.

       01  WSAA-OKEY.
           03  WSAA-OKEY1              PIC S9(04) COMP-3.
           03  FILLER                  PIC X(26).

       01  WSAA-FOUND                  PIC X(01).

      *  Make this field as large as you think the data which
      *  will be stored in it is ever likely to be. Calcualate
      *  carefully as too small will result in truncation and too
      *  large will result in poor performance.

       01  WSAA-DATA-BUFFER            PIC X(5000) VALUE SPACES.

      * The following array is configured to store up to 3 fields,
      * ensure you change it to store exactly how many fields are
      * stored/retrieved by this subroutine.

       01  OFFSET                      PIC S9(03) COMP-3.
       01  FILLER.
           03  WSAA-START-AND-LENGTH   OCCURS 100.
               05  STRPOS              PIC S9(05) COMP-3.
               05  FLDLEN              PIC S9(05) COMP-3.
      *
       01  FORMATS.
           03  ZPPIENQREC              PIC X(10) VALUE 'ZPPIENQREC'.
      *
           COPY ZPPIENQSKM.
           COPY ZPRMYRREC.
           COPY SYSERRREC.
           COPY VARCOM.

       LINKAGE SECTION.

           COPY PCPDATAREC.
           COPY LETCSKM.

       PROCEDURE DIVISION USING PCPD-PCPDATA-REC LETC-PARAMS.
       000-MAIN SECTION.
      *****************
       010-MAIN.

           IF  PCPD-IDCODE-COUNT   NOT = WSAA-LAST-IDCODE-COUNT
               PERFORM 100-INITIALISE
               PERFORM 200-GET-DETAILS
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
      ************************
       101-PARA.
      *
           MOVE PCPD-IDCODE-COUNT      TO WSAA-LAST-IDCODE-COUNT.
           MOVE 'N'                    TO WSAA-FOUND.
      *
           MOVE ZEROES                 TO WSAA-AMOUNT-X.

           INITIALIZE                     ZPPIENQ-PARAMS.
           MOVE LETC-CHDRNUM           TO ZPPIENQ-CHDRNUM.
           MOVE READR                  TO ZPPIENQ-FUNCTION.
           MOVE ZPPIENQREC             TO ZPPIENQ-FORMAT.

           CALL 'ZPPIENQIO'         USING ZPPIENQ-PARAMS.

           IF ZPPIENQ-STATUZ        NOT = O-K AND MRNF
              MOVE ZPPIENQ-STATUZ      TO SYSR-STATUZ
              MOVE ZPPIENQ-PARAMS      TO SYSR-PARAMS
              PERFORM 900-FATAL-ERROR
           END-IF.

           IF ZPPIENQ-STATUZ            = O-K
              MOVE 'Y'                 TO WSAA-FOUND
           END-IF.
      *
       109-EXIT.
           EXIT.

       200-GET-DETAILS SECTION.
      *************************
      *
       201-PLAN-PREMIUM-1.
      *    Field 01 - Plan Premium of 1st.

           IF WSAA-FOUND            NOT = 'Y'
              MOVE ZEROES              TO ZPPIENQ-PREMESTA
                                          ZPPIENQ-PREMESTB
                                          ZPPIENQ-PREMESTC
                                          ZPPIENQ-PREMESTD
           END-IF.
      *
           MOVE 1                      TO  OFFSET.
           MOVE 1                      TO  STRPOS.
      *
           MOVE ZPPIENQ-PREMESTA       TO WSAA-AMOUNT-X.
           MOVE LENGTH OF WSAA-AMOUNT-X
                                       TO FLDLEN(OFFSET)
           MOVE WSAA-AMOUNT-X
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):
                                       FLDLEN(OFFSET)).
      *
       202-FORMAT.
      *    Field 02 - Plan Premium of 2nd.

           ADD 1                       TO  OFFSET.
           COMPUTE STRPOS(OFFSET)       =
                   STRPOS(OFFSET - 1)   + FLDLEN(OFFSET - 1).

           MOVE ZPPIENQ-PREMESTB       TO WSAA-AMOUNT-X.
           MOVE LENGTH OF WSAA-AMOUNT-X
                                       TO FLDLEN(OFFSET)
           MOVE WSAA-AMOUNT-X
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):
                                       FLDLEN(OFFSET)).
      *
       203-FORMAT.
      *    Field 03 - Plan Premium of 3rd.

           ADD 1                       TO  OFFSET.
           COMPUTE STRPOS(OFFSET)       =
                   STRPOS(OFFSET - 1)   + FLDLEN(OFFSET - 1).

           MOVE ZPPIENQ-PREMESTC       TO WSAA-AMOUNT-X.
           MOVE LENGTH OF WSAA-AMOUNT-X
                                       TO FLDLEN(OFFSET)
           MOVE WSAA-AMOUNT-X
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):
                                       FLDLEN(OFFSET)).
      *
       204-FORMAT.
      *    Field 04 - Plan Premium of 4th.

           ADD 1                       TO  OFFSET.
           COMPUTE STRPOS(OFFSET)       =
                   STRPOS(OFFSET - 1)   + FLDLEN(OFFSET - 1).

           MOVE ZPPIENQ-PREMESTD       TO WSAA-AMOUNT-X.
           MOVE LENGTH OF WSAA-AMOUNT-X
                                       TO FLDLEN(OFFSET)
           MOVE WSAA-AMOUNT-X
             TO WSAA-DATA-BUFFER(STRPOS(OFFSET):
                                       FLDLEN(OFFSET)).
      *
       299-EXIT.
           EXIT.
      /
       900-FATAL-ERROR SECTION.
      ************************
       910-FATAL.
           MOVE WSAA-SUBRNAME          TO SYSR-SUBRNAME.
           MOVE BOMB                   TO PCPD-STATUZ.
           IF  SYSR-STATUZ             NOT = SPACES
           OR  SYSR-SYSERR-STATUZ      NOT = SPACES
              MOVE '1'                 TO SYSR-SYSERR-TYPE
           ELSE
              MOVE '2'                 TO SYSR-SYSERR-TYPE.
           CALL 'SYSERR'               USING SYSR-SYSERR-REC.
      /
       990-EXIT.
           EXIT PROGRAM.
