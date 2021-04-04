       IDENTIFICATION DIVISION.
       PROGRAM-ID. VLPDSQNO.
      *
      * Copyright 1986-2021, Computer Sciences Corporation.
      *
      *
      *REMARKS.
      *
      * VALIDATE SEQUENCE OF LIFE AND COVERAGE IS COTINUOUS.
      *
      *
      ***********************************************************************
      *           AMENDMENT  HISTORY                                        *
      ***********************************************************************
      * DATE.... VSN/MOD  WORK UNIT    BY....                               *
      *                                                                     *
      * 02/04/21  01/01   DUMMY        IT-Nguyen Huu Tri                    *
      *           VALIDATE SEQUENCE OF LIFE AND COVERAGE IS COTINUOUS.      *
      *                                                                     *
      **DD/MM/YY*************************************************************
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.                                IBM-AS400.
       OBJECT-COMPUTER.                                IBM-AS400.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      /
       DATA DIVISION.
       FILE SECTION.
      /
       WORKING-STORAGE SECTION.
      *
       01  WSAA-SUBR                   PIC X(08) VALUE 'VLPDSQNO'.
       01  WSAA-VERSION                PIC X(02) VALUE '01'.
      *
       01  FORMATS.
           03  COVTCSNREC              PIC X(10) VALUE 'COVTCSNREC'.
      *
      *01  TABLES.
      *    03  T1692                   PIC X(06) VALUE 'T1692'.
      *    03  T1693                   PIC X(06) VALUE 'T1693'.
      *    03  T3629                   PIC X(06) VALUE 'T3629'.
      *
       01  ERRORS.
           03  EZ81                    PIC X(04) VALUE 'EZ81'.
           03  EZ82                    PIC X(04) VALUE 'EZ82'.
      *
      *
       01  IDX                         PIC 9(02) VALUE 0.
       01  WSAA-CHDRNUMTMP             PIC X(08).
       01  WSAA-CNTLIFE                PIC X(02).
       01  WSAA-CNTCOVR                PIC X(02).
       01  WSAA-CNTLIFE-R  REDEFINES   WSAA-CNTLIFE PIC 9(02).
       01  WSAA-CNTCOVR-R  REDEFINES   WSAA-CNTCOVR PIC 9(02).
       01  WSAA-END                    PIC X(01).
       01  WSAA-ERRCODE                PIC X(04).
       01  WSAA-ISFIRSTROW             PIC X(01).
      *
      *
      /
           COPY CONLOGREC.
           COPY DATCON1REC.
           COPY SFTLOCKREC.

           COPY SYSERRREC.
           COPY VARCOM.

           COPY COVTCSNSKM.
      /
      *
       LINKAGE SECTION.
      *****************
      *
           COPY VLPDSUBREC.
      /
       PROCEDURE DIVISION           USING VLSB-VALID-REC.
      *
       000-MAIN SECTION.
      ******************
       010-START.

           INITIALIZE                     COVTCSN-PARAMS.
           MOVE 'N'                    TO WSAA-END.
           MOVE O-K                    TO VLSB-STATUZ.
           MOVE ZEROES                 TO IDX.
           MOVE '00'                   TO WSAA-CNTLIFE.
           MOVE '00'                   TO WSAA-CNTCOVR.
           MOVE VLSB-CHDRCOY           TO COVTCSN-CHDRCOY.
           MOVE VLSB-CHDRNUM           TO COVTCSN-CHDRNUM.
           MOVE SPACES                 TO COVTCSN-LIFE.
           MOVE SPACES                 TO COVTCSN-COVERAGE.
           MOVE SPACES                 TO COVTCSN-RIDER.
           MOVE 1                      TO COVTCSN-SEQNBR.
           MOVE COVTCSNREC             TO COVTCSN-FORMAT.
           MOVE BEGN                   TO COVTCSN-FUNCTION.

           PERFORM 200-CHECK-SEQUENCE  UNTIL WSAA-END = 'Y'.

       090-EXIT.
           EXIT PROGRAM.
      /
      *
       200-CHECK-SEQUENCE SECTION.
      *******************************
       200-START.

           CALL 'COVTCSNIO'         USING COVTCSN-PARAMS.
           IF COVTCSN-STATUZ          NOT = O-K AND 'ENDP'

              MOVE COVTCSN-PARAMS      TO SYSR-PARAMS
              PERFORM 600-FATAL-ERROR

           END-IF.

           IF COVTCSN-STATUZ              = 'ENDP'

              MOVE  'Y'                TO WSAA-END
              GO TO 210-EXIT

           END-IF.

           IF COVTCSN-CHDRNUM         NOT = VLSB-CHDRNUM

              MOVE 'Y'                 TO WSAA-END
              GO TO 210-EXIT

           END-IF.
      
           IF WSAA-ISFIRSTROW                 = 'Y'

               IF COVTCSN-LIFE            NOT = '01'
    
                  MOVE EZ81                TO  WSAA-ERRCODE
                  PERFORM 500-MOVE-ERROR 
                  MOVE 'Y'                 TO  WSAA-END 
                  GO TO 210-EXIT
    
               END-IF
    
               ELSE IF COVTCSN-COVERAGE        NOT = '01'
    
                  MOVE EZ82                TO  WSAA-ERRCODE
                  PERFORM 500-MOVE-ERROR 
                  MOVE 'Y'                 TO WSAA-END
                  GO TO 210-EXIT
              
               END-IF

           END-IF.    
           MOVE 'N'                 TO WSAA-ISFIRSTROW  
           PERFORM 300-CHECK-SEQUENCE-LIFE.

           MOVE NEXTR                  TO COVTCSN-FUNCTION.

        210-EXIT.
            EXIT.
      *
       300-CHECK-SEQUENCE-LIFE SECTION.
      ************************************
       300-START.
           ADD 1                       TO WSAA-CNTLIFE-R
           IF COVTCSN-LIFE                = WSAA-CNTLIFE
              ADD 1                    TO WSAA-CNTCOVR-R
              IF COVTCSN-COVERAGE    NOT  = WSAA-CNTCOVR

                 MOVE EZ82            TO    WSAA-ERRCODE
                 PERFORM 500-MOVE-ERROR
                 MOVE 'Y'             TO    WSAA-END
                 GO TO 310-EXIT

              ELSE

                 SUBTRACT 1 FROM WSAA-CNTCOVR-R
                 ADD 1 TO  WSAA-CNTCOVR-R

              END-IF

              SUBTRACT 1  FROM WSAA-CNTLIFE-R

           ELSE
              SUBTRACT 1  FROM WSAA-CNTLIFE-R
              ADD 2 TO WSAA-CNTLIFE-R
              IF COVTCSN-LIFE          =  WSAA-CNTLIFE

                 SUBTRACT 2 FROM WSAA-CNTLIFE-R
                 ADD 1 TO WSAA-CNTLIFE-R
                 MOVE ZEROES TO WSAA-CNTCOVR-R
                 GO TO 300-START

             ELSE

                 MOVE EZ81                TO  WSAA-ERRCODE
                 PERFORM 500-MOVE-ERROR
                 MOVE 'Y'                 TO  WSAA-END
                 GO TO 310-EXIT

             END-IF

           END-IF.

       310-EXIT.
           EXIT.
      *
       500-MOVE-ERROR SECTION.
      ************************
       510-BEGIN.
      *
           ADD 1                       TO IDX.
           MOVE WSAA-ERRCODE           TO VLSB-ERR-CODE (IDX).
      *
       590-EXIT.
           EXIT.
      *




      **************************
       600-FATAL-ERROR SECTION.
      **************************
       610-FATAL-ERRORS.

           MOVE WSAA-SUBR              TO SYSR-SUBRNAME.
           MOVE SYSR-STATUZ            TO VLSB-STATUZ.
      *
           IF  SYSR-STATUZ             =  BOMB
               GO TO 690-EXIT
           END-IF.
      *
           MOVE SYSR-STATUZ            TO SYSR-SYSERR-STATUZ.
      *
           IF  SYSR-SYSERR-TYPE        NOT =  '2'
               MOVE '1'                TO SYSR-SYSERR-TYPE
           END-IF.
      *
           CALL 'SYSERR'               USING SYSR-SYSERR-REC.
      *
       690-EXIT.
           EXIT PROGRAM.


