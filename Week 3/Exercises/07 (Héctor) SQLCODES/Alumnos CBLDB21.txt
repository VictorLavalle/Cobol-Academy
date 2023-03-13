      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CBLDB21
       AUTHOR.        Xideral.

       ENVIRONMENT DIVISION.
      *--------------------
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPOUT
                  ASSIGN TO UT-S-REPORT.

       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  REPOUT
               RECORD CONTAINS 69 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS REPREC.

       01  REPREC.
           05  ACCT-ID-O      PIC X(8).
           05  ACCT-LIMITE-O  PIC $$,$$$,$$9.99.
           05  ACCT-SALDO-O   PIC $$,$$$,$$9.99.
           05  ACCT-APEP-O    PIC X(20).
           05  ACCT-NOMBRE-O  PIC X(15).

       WORKING-STORAGE SECTION.
       01 FLAGS.
         05 BANDERA-CURSOR          PIC X VALUE SPACE.
               88  FIN-CURSOR        VALUE 'Y'.
               88  NO-FIN-CURSOR     VALUE 'N'.
      *****************************************************
                EXEC SQL INCLUDE SQLCA  END-EXEC.
      *****************************************************
       01 UD-ERROR-MESSAGE   PIC X(80)  VALUE SPACES.
       01  SQLCODES.
           05 SQLCODE0             PIC S9(9) COMP-5 VALUE 0.
           05 SQLCODE100           PIC S9(9) COMP-5 VALUE 100.
      *****************************************************
      * DECLARACION SQL DE LA TABLA                       *
      *****************************************************
                EXEC SQL DECLARE Z94379T  TABLE
                        (ACCTNO     CHAR(8)  NOT NULL,
                         LIMIT      DECIMAL(9,2)     ,
                         BALANCE    DECIMAL(9,2)     ,
                         SURNAME    CHAR(20) NOT NULL,
                         FIRSTN     CHAR(15) NOT NULL,
                         ADDRESS1   CHAR(25) NOT NULL,
                         ADDRESS2   CHAR(20) NOT NULL,
                         ADDRESS3   CHAR(15) NOT NULL,
                         RESERVED   CHAR(7)  NOT NULL,
                         COMMENTS   CHAR(50) NOT NULL)
                         END-EXEC.
      *****************************************************
      * SQL CURSORS                                       *
      *****************************************************
                EXEC SQL DECLARE CURTABLA  CURSOR FOR
                         SELECT * FROM Z94379T
                 END-EXEC.
      *****************************************************
      * VARIABLES HOST EN DONDE RECIBIMOS LA TABLA        *
      *****************************************************
       01 VARIABLES-HOST.
          02 ACCT-ID            PIC X(8).
          02 ACCT-LIMITE        PIC S9(7)V99 COMP-3.
          02 ACCT-SALDO         PIC S9(7)V99 COMP-3.
          02 ACCT-APEP          PIC X(20).
          02 ACCT-NOMBRE        PIC X(15).
          02 ACCT-DIRE1         PIC X(25).
          02 ACCT-DIRE2         PIC X(20).
          02 ACCT-DIRE3         PIC X(15).
          02 ACCT-RESER         PIC X(7).
          02 ACCT-COMENT        PIC X(50).

       PROCEDURE DIVISION.
       EMPIEZO-PROGRAMA.
                OPEN OUTPUT REPOUT.
                SET NO-FIN-CURSOR TO TRUE
                PERFORM PROCESO-PRINCIPAL.

       FIN-PROGRAMA.
                CLOSE REPOUT.
                GOBACK.

       PROCESO-PRINCIPAL.
                PERFORM ABRO-CURSOR
                PERFORM LEO-CURSOR
                PERFORM CICLO-CURSOR UNTIL FIN-CURSOR
                PERFORM CIERRO-CURSOR.
                
       CICLO-CURSOR.
                PERFORM ESCRIBE-REPORTE.
                PERFORM LEO-CURSOR.

       ESCRIBE-REPORTE.
                MOVE  ACCT-ID      TO  ACCT-ID-O.
                MOVE  ACCT-LIMITE  TO  ACCT-LIMITE-O.
                MOVE  ACCT-SALDO   TO  ACCT-SALDO-O.
                MOVE  ACCT-APEP    TO  ACCT-APEP-O.
                MOVE  ACCT-NOMBRE  TO  ACCT-NOMBRE-O.
                WRITE REPREC AFTER ADVANCING 2 LINES.

       EVALUO-SQLCODES.    
           EVALUATE SQLCODE
              WHEN SQLCODE0
                   SET NO-FIN-CURSOR TO TRUE
              WHEN SQLCODE100
                   SET FIN-CURSOR TO TRUE
              WHEN OTHER    
                   MOVE 'ERROR EN CURSOR' TO UD-ERROR-MESSAGE
                   STOP RUN 
           END-EVALUATE.   

       ABRO-CURSOR.
           EXEC SQL
              OPEN CURTABLA
           END-EXEC.
           PERFORM EVALUO-SQLCODES.

       LEO-CURSOR.    
           EXEC SQL 
              FETCH CURTABLA 
              INTO :VARIABLES-HOST
           END-EXEC.
           PERFORM EVALUO-SQLCODES.

       CIERRO-CURSOR.
           EXEC SQL 
              CLOSE CURTABLA  
           END-EXEC.
           PERFORM EVALUO-SQLCODES.