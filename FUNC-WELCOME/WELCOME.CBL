       IDENTIFICATION DIVISION.
       PROGRAM-ID. WELCOME.
      *
      * Copyright 1986-2020, Computer Sciences Corporation.
      *
      *REMARKS.
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 16/12/20  01/01   DUMMY        IT-Nguyen Huu Tri                    *
      *           GET DATA FOR WELCOME LETTER.                              *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
      ***********************OFFSET DESCRIPTION***********************

      * 01 STAFF ID.
      * 02 STAFF FULLNAME.
      * 03 STAFF LOCATION.

      *****************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                                IBM-AS400.
       OBJECT-COMPUTER.                                IBM-AS400.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      *
       01  WSAA-SUBRNAME               PIC X(07) VALUE 'WELCOME'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
       01  WSAA-STORE-RRN              PIC S9(09) COMP-3.
       01  WSAA-DATE                   PIC X(10).
       01  WSAA-DATA-BUFFER            PIC X(200) VALUE SPACES.
       01  WSAA-FULLNAME               PIC X(100).

      *
       01  OFFSET                      PIC S9(03) COMP-3.
       01  FILLER.
           03  WSAA-START-AND-LENGTH   OCCURS  3.
               05  STRPOS              PIC S9(05) COMP-3.
               05  FLDLEN              PIC S9(05) COMP-3.

      *
       01  FORMATS.
           03  ZZZ4REC                 PIC X(07) VALUE 'ZZZ4REC'.

      *
           COPY SYSERRREC.
           COPY VARCOM.
           COPY DATCON1REC.
           COPY ZZZ4SKM.

       LINKAGE SECTION.

           COPY PCPDATAREC.
           COPY LETCSKM.

       PROCEDURE DIVISION USING PCPD-PCPDATA-REC LETC-PARAMS.
       000-MAIN SECTION.
       010-MAIN.
           IF  LETC-RRN  NOT = WSAA-STORE-RRN

               PERFORM 100-INITIALISE
               PERFORM 200-GET-ZZZ4-AND-FORMAT

           END-IF.

           MOVE PCPD-FLD-OFFSET        TO OFFSET.
           MOVE WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET))
                                       TO PCPD-DATA.
           MOVE FLDLEN(OFFSET)         TO PCPD-DATA-LEN.
           MOVE O-K                    TO PCPD-STATUZ.

       090-EXIT.
           EXIT PROGRAM.

       100-INITIALISE SECTION.
       101-PARA.

      *    MOVE PCPD-IDCODE-COUNT      TO WSAA-LAST-IDCODE-COUNT.
           MOVE LETC-RRN               TO WSAA-STORE-RRN.
           MOVE SPACES                 TO WSAA-FULLNAME.

       109-EXIT.
           EXIT.

       200-GET-ZZZ4-AND-FORMAT SECTION.
      *********************************
       200-GET-DATA.

           MOVE  LETC-CLNTNUM          TO ZZZ4-TAGNTNUM.
           MOVE  READR                 TO ZZZ4-FUNCTION.
           CALL 'ZZZ4IO'            USING ZZZ4-PARAMS.
           IF  ZZZ4-STATUZ            NOT = O-K AND MRNF

               MOVE ZZZ4-PARAMS        TO SYSR-PARAMS
               PERFORM 600-FATAL-ERROR

           END-IF.

       201-FORMAT-STAFFID.

           MOVE 1                      TO OFFSET.
           MOVE 1                      TO STRPOS(OFFSET).
           MOVE LENGTH OF ZZZ4-TAGNTNUM TO FLDLEN(OFFSET).
           MOVE ZZZ4-TAGNTNUM
                TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       202-FORMAT-STAFF-FULLNAME.

           ADD 1                       TO OFFSET.
           STRING
                 ZZZ4-TFNAME DELIMITED BY '  '
                 SPACES ZZZ4-TLNAME DELIMITED BY SIZE
                 INTO WSAA-FULLNAME
           END-STRING.
           COMPUTE STRPOS(OFFSET) =
                 STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE LENGTH OF WSAA-FULLNAME TO FLDLEN(OFFSET).
           MOVE WSAA-FULLNAME
                TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       203-FORMAT-STAFF-LOCATION.

           ADD 1                       TO OFFSET.
           COMPUTE STRPOS(OFFSET) =
                 STRPOS(OFFSET - 1) + FLDLEN(OFFSET - 1).
           MOVE LENGTH OF ZZZ4-TAREACODE
                                       TO FLDLEN(OFFSET).
           MOVE ZZZ4-TAREACODE
                TO WSAA-DATA-BUFFER(STRPOS(OFFSET):FLDLEN(OFFSET)).

       210-EXIT.
           EXIT.

       600-FATAL-ERROR SECTION.
       610-FATAL.

           MOVE WSAA-SUBRNAME          TO SYSR-SUBRNAME.
           MOVE BOMB                   TO PCPD-STATUZ.

           IF SYSR-STATUZ             NOT = SPACES
           OR  SYSR-SYSERR-STATUZ      NOT = SPACES

               MOVE '1'                 TO SYSR-SYSERR-TYPE

           ELSE

               MOVE '2'                TO SYSR-SYSERR-TYPE

           END-IF.

           CALL 'SYSERR'               USING SYSR-SYSERR-REC.

       690-EXIT.
           EXIT PROGRAM.

