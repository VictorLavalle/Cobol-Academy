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

      *****************************************************
      * Print format for the report                       *
      *****************************************************
       01  HEADER-1.
           05 FILLER PIC X(11) VALUE '-------- '.
           05 FILLER PIC X(17) VALUE 'TABLE STATISTICS '.
           05 FILLER PIC X(10) VALUE '---------'.

       01 FOOTER-DATA-1.
          05 FILLER            PIC X(20)         VALUE
                'Money total amount: '.
          05 TOTAL-MONEY       PIC $$$,$$$,$$9.99.

       01 FOOTER-DATA-2.
          05 FILLER            PIC X(23)         VALUE
                'Num. Virginia clients: '.
          05 TOTAL-VIR         PIC Z9.

       01 FOOTER-DATA-3.
          05 FILLER            PIC X(22)         VALUE
                'Num. Chicago clients: '.
          05 TOTAL-CHI         PIC Z9.

       01  FOOTER-LINES.
           05 FILLER  PIC X(19) VALUE '-------------------'.
           05 FILLER  PIC X(19) VALUE '-------------------'.



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
      *
      * New variables to calculate the total amount of clients
      * from Chicago (CHI) and Virgniia (VIR)
      *
          02 WSV-COUNT-CHI     PIC S9(9)V99 COMP-3.
          02 WSV-COUNT-VIR     PIC S9(9)V99 COMP-3.
          02 WSV-SUM-MONEY     PIC S9(9)V99 COMP-3.

       01 STATES.
          05 WSC-VIR-STATE         PIC X(10)         VALUE 'Virginia'.
          05 WSC-CHI-STATE         PIC X(10)         VALUE 'Chicago'.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*

      ****************************************************
      * The program starts with a procedure division and *
      * a main process named "START-PROGRAM".         *
      ****************************************************
       START-PROGRAM.
           OPEN OUTPUT REPOUT.
      *     SET NO-END-CURSOR TO TRUE
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

           PERFORM CALCULATE-TOTAL-MONEY
           PERFORM COUNT-CHI-CLIENTS
           PERFORM COUNT-VIR-CLIENTS

           PERFORM WRITE-REPORT.





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

           MOVE WSV-SUM-MONEY TO TOTAL-MONEY
           MOVE WSV-COUNT-CHI TO TOTAL-CHI
           MOVE WSV-COUNT-VIR TO TOTAL-VIR

           WRITE REPREC FROM HEADER-1
           WRITE REPREC FROM FOOTER-DATA-1
           WRITE REPREC FROM FOOTER-DATA-2
           WRITE REPREC FROM FOOTER-DATA-3
           WRITE REPREC FROM FOOTER-LINES AFTER ADVANCING 1 LINE.


      ****************************************************
      * The EVALUO-SQLCODES subroutine evaluates the     *
      * SQLCODE value and sets flags accordingly.        *
      ****************************************************
       EVALUATE-SQLCODES.
           EVALUATE SQLCODE
           WHEN SQLCODE0
                SET NO-END-CURSOR TO TRUE
                DISPLAY 'QUERY SUCCESS ' SQLCODE
           WHEN SQLCODE100
                SET END-CURSOR TO TRUE
                DISPLAY 'REGISTER NOT FOUND ' SQLCODE
                STOP RUN
           WHEN OTHER
                MOVE 'ERROR IN CURSOR' TO UD-ERROR-MESSAGE
                STOP RUN
           END-EVALUATE.



      ******************************************************
      * The CALCULATE-TOTAL-MONEY subroutine performs the  *
      * sum of all the balance of all the registers and    *
      * evaluate the SQLCODE value.                        *
      ******************************************************

       CALCULATE-TOTAL-MONEY.

           EXEC SQL
                SELECT SUM(BALANCE)
                INTO :WSV-SUM-MONEY
                FROM Z94474T
                END-EXEC

           PERFORM EVALUATE-SQLCODES.


      ******************************************************
      * The COUNT-VIR-CLIENTS subroutine performs the   *
      * sum of all the balance of all the registers and    *
      * evaluate the SQLCODE value.                        *
      ******************************************************
       COUNT-VIR-CLIENTS.
           EXEC SQL
                SELECT COUNT(*)
                INTO :WSV-COUNT-VIR
                FROM Z94474T
                WHERE ADDRESS3 = :WSC-VIR-STATE
                END-EXEC

           PERFORM EVALUATE-SQLCODES.


      ******************************************************
      * The COUNT-VIR-CLIENTS subroutine performs the   *
      * sum of all the balance of all the registers and    *
      * evaluate the SQLCODE value.                        *
      ******************************************************
       COUNT-CHI-CLIENTS.
           EXEC SQL
                SELECT COUNT(*)
                INTO :WSV-COUNT-VIR
                FROM Z94474T
                WHERE ADDRESS3 = :WSC-CHI-STATE
                END-EXEC

           PERFORM EVALUATE-SQLCODES.
