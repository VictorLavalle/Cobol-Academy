      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBLDB21
       AUTHOR.        Xideral.

      *________________________________________________________________*
      *                                                                *
      *                     "Practice 7"                               *
      *                                                                *
      * Instructions: Handle errors with SQLCODES                      *
      *                                                                *
      *________________________________________________________________*



      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPOUT
                  ASSIGN TO UT-S-REPORT.



      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       FILE SECTION.
       FD  REPOUT
               RECORD CONTAINS 69 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS REPREC.

       01 REPREC.
          05 ACCT-ID-O         PIC X(8).
          05 ACCT-LIMIT-O      PIC $$,$$$,$$9.99.
          05 ACCT-BALANCE-O    PIC $$,$$$,$$9.99.
          05 ACCT-APEP-O       PIC X(20).
          05 ACCT-NAME-O       PIC X(15).



       WORKING-STORAGE SECTION.
       01 FLAGS.
          05 FLAG-CURSOR       PIC X             VALUE SPACE.
             88 END-CURSOR                       VALUE 'Y'.
             88 NO-END-CURSOR                    VALUE 'N'.

       01 CLIENT-TABLE.
          05 WSV-SUM-MONEY     PIC S9(9)V99 COMP-3.


      *****************************************************
                EXEC SQL INCLUDE SQLCA  END-EXEC.
      *****************************************************
       01 UD-ERROR-MESSAGE     PIC X(80)         VALUE SPACES.
       01 SQLCODES.
          05 SQLCODE0          PIC S9(9) COMP-5  VALUE 0.
          05 SQLCODE100        PIC S9(9) COMP-5  VALUE 100.


      *****************************************************
      *  SQL Table Declaration                            *
      *****************************************************
                EXEC SQL DECLARE Z94474T TABLE
                        (ACCTNO CHAR(8) NOT NULL,
                         LIMIT DECIMAL(9,2) ,
                         BALANCE DECIMAL(9,2) ,
                         SURNAME CHAR(20) NOT NULL,
                         FIRSTN CHAR(15) NOT NULL,
                         ADDRESS1 CHAR(25) NOT NULL,
                         ADDRESS2 CHAR(20) NOT NULL,
                         ADDRESS3 CHAR(15) NOT NULL,
                         RESERVED CHAR(7) NOT NULL,
                         COMMENTS CHAR(50) NOT NULL)
                         END-EXEC.


      *****************************************************
      * SQL Cursors                                       *
      *****************************************************
           EXEC SQL
            DECLARE CURTABLE CURSOR FOR SELECT * FROM Z94474T
           END-EXEC.

      *****************************************************
      * HOST variables where we receive the table         *
      *****************************************************
       01 HOST-VARIABLES.
          02 ACCT-ID           PIC X(8).
          02 ACCT-LIMIT        PIC S9(7)V99 COMP-3.
          02 ACCT-BALANCE      PIC S9(7)V99 COMP-3.
          02 ACCT-APEP         PIC X(20).
          02 ACCT-NAME         PIC X(15).
          02 ACCT-DIRE1        PIC X(25).
          02 ACCT-DIRE2        PIC X(20).
          02 ACCT-DIRE3        PIC X(15).
          02 ACCT-RESER        PIC X(7).
          02 ACCT-COMMENT      PIC X(50).


      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      ****************************************************
      * The program starts with a procedure division and *
      * a main process named "START-PROGRAM".         *
      ****************************************************
       START-PROGRAM.
           OPEN OUTPUT REPOUT.
           SET NO-END-CURSOR TO TRUE
           PERFORM MAIN-PROCESS.


      ****************************************************
      * The program ends with the END-PROGRAM process,  *
      * which closes the REPOUT file and returns to the  *
      * calling program.                                 *
      ****************************************************
       END-PROGRAM.
           CLOSE REPOUT.
           GOBACK.


      ***************************************************
      * The main process, named PROCESO-PRINCIPAL,      *
      * calls four subroutines in order:                *
      *                                                 *
      * OPEN-CURSOR, READ-CURSOR, LOOP-CURSOR, and      *
      * CLOSE-CURSOR.                                  *
      ***************************************************
       MAIN-PROCESS.
           PERFORM OPEN-CURSOR
           PERFORM READ-CURSOR
           PERFORM CALCULATE-TOTAL-MONEY
           PERFORM LOOP-CURSOR UNTIL END-CURSOR
           PERFORM CLOSE-CURSOR.



      ***************************************************
      * The LOOP-CURSOR subroutine calls the           *
      * WRITE-REPORT subroutine and the READ-CURSOR   *
      *s ubroutine.                                     *
      ***************************************************
       LOOP-CURSOR.
           PERFORM WRITE-REPORT.
           PERFORM READ-CURSOR.



      ***************************************************
      * The WRITE-REPORT subroutine moves data from  *
      * several variables to other variables and writes *
      * them to the REPOUT file.                        *
      ***************************************************
       WRITE-REPORT.
           MOVE ACCT-ID TO ACCT-ID-O
           MOVE ACCT-LIMIT TO ACCT-LIMIT-O
           MOVE ACCT-BALANCE TO ACCT-BALANCE-O
           MOVE ACCT-APEP TO ACCT-APEP-O
           MOVE ACCT-NAME TO ACCT-NAME-O
           WRITE REPREC AFTER ADVANCING 2 LINES.



      ****************************************************
      * The EVALUO-SQLCODES subroutine evaluates the     *
      * SQLCODE value and sets flags accordingly.        *
      ****************************************************
       EVALUATE-SQLCODES.
           EVALUATE SQLCODE
           WHEN SQLCODE0
                SET NO-END-CURSOR TO TRUE
           WHEN SQLCODE100
                SET END-CURSOR TO TRUE
           WHEN OTHER
                MOVE 'ERROR EN CURSOR' TO UD-ERROR-MESSAGE
                STOP RUN
           END-EVALUATE.


      ******************************************************
      * The OPEN-CURSOR subroutine opens the CURTABLE      *
      * cursor and calls the EVALUO-SQLCODES subroutine to *
      *evaluate the SQLCODE value.                         *
      ******************************************************
       OPEN-CURSOR.
           EXEC SQL
                OPEN CURTABLE
                END-EXEC.
           PERFORM EVALUATE-SQLCODES.


      ******************************************************
      * The READ-CURSOR subroutine fetches data from the   *
      * CURTABLE cursor and calls the EVALUO-SQLCODES      *
      *subroutine to evaluate the SQLCODE value.           *
      ******************************************************
       READ-CURSOR.
           EXEC SQL
                FETCH CURTABLE
                INTO :HOST-VARIABLES
                END-EXEC.
           PERFORM EVALUATE-SQLCODES.


      ******************************************************
      * The CLOSE-CURSOR subroutine closes the CURTABLE    *
      * cursor and calls the EVALUO-SQLCODES subroutine to *
      * evaluate the SQLCODE value.                        *
      ******************************************************
       CLOSE-CURSOR.
           EXEC SQL
                CLOSE CURTABLE
                END-EXEC.
           PERFORM EVALUATE-SQLCODES.


