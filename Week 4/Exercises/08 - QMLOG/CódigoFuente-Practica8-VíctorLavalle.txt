      *----------------------------------------------------------------*
       IDENTIFICATION DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.    CBLDB21
       AUTHOR.        Victor Lavalle.

      *________________________________________________________________*
      *                                                                *
      *                     "Practice 8"                               *
      *                                                                *
      * Instructions: Handle errors with SQLCODES, Commits and Rollback*
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
               RECORD CONTAINS 46 CHARACTERS
               LABEL RECORDS ARE OMITTED
               DATA RECORD IS REPREC.

       01 REPREC.
          05 ACCT-NOMBRE-O          PIC X(15).
          05 ACCT-STATE-O           PIC X(18).
          05 ACCT-SALDO-O           PIC $$,$$$,$$9.99.


       WORKING-STORAGE SECTION.
       01 FLAGS.
          05 BANDERA-CURSOR         PIC X             VALUE SPACE.
             88 FIN-CURSOR                            VALUE 'Y'.
             88 NO-FIN-CURSOR                         VALUE 'N'.
      *****************************************************
                EXEC SQL INCLUDE SQLCA  END-EXEC.
      *****************************************************
       01 UD-ERROR-MESSAGE          PIC X(80)         VALUE SPACES.
       01 SQLCODES.
          05 SQLCODE0               PIC S9(9) COMP-5  VALUE 0.
          05 SQLCODE100             PIC S9(9) COMP-5  VALUE 100.

      *****************************************************
      * DECLARACION SQL DE LA TABLA                       *
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
      * SQL CURSORS                                       *
      *****************************************************
                EXEC SQL DECLARE CURTABLA CURSOR FOR
                        SELECT FIRSTN, ADDRESS3, BALANCE FROM Z94474T
                        ORDER BY BALANCE DESC LIMIT 3
                 END-EXEC.

      *****************************************************
      * VARIABLES HOST EN DONDE RECIBIMOS LA TABLA        *
      *****************************************************
       01 VARIABLES-HOST.
          02 ACCT-NOMBRE            PIC X(15).
          02 ACCT-DIRE3             PIC X(15).
          02 ACCT-SALDO             PIC S9(07)V99 COMP-3.

       01 UPDATE-CONSTANTS.
          05 WSC-UPD-NY-STATE       PIC X(08)         VALUE "New York".

       01 CALCULATE-VARIABLES.
          05 WSV-TOTAL-AVG          PIC S9(7)V99 COMP-3.
          05 WSV-PARG-NAME          PIC X(16).


       01 LOG-MESSAGES.
          05 WSV-LOG-MESSAGE        PIC X(80)         VALUE SPACES.
          05  QMLOG-FD              PIC S9(4) BINARY  VALUE 0.
          05  QMLOG-RECORD          PIC X(80)         VALUE SPACES.


       01 ERROR-MESSAGES.
          05 WSC-ERR-MSG-CUR        PIC X(28)         VALUE
                "Error en operacion de cursor".

          05 WSC-ERR-MSG-NOT-FOUND  PIC X(22)         VALUE
                "Registro no encontrado".

          05 WSC-ERR-MSG-EMPTY      PIC X(16)         VALUE
                "Registro vacio".

          05 WSC-ERR-MSG-PARG       PIC X(16)         VALUE
                "Error en parrafo".


       01 SUCCESS-MESSAGES.
          05 WSC-SUC-MSG-UPD        PIC X(21)         VALUE
                "Actualizacion exitosa".

          05 WSC-SUC-MSG-DEL        PIC X(19)         VALUE
                "Eliminacion exitosa".

          05 WSC-SUC-MSG-INS        PIC X(17)         VALUE
                "Insercion exitosa".

          05 WSC-SUC-MSG-COMMIT     PIC X(14)         VALUE
                "Commit exitoso".

          05 WSC-SUC-MSG-ROLLBACK   PIC X(16)         VALUE
                "Rollback exitoso".

          05 WSC-SUC-MSG-READ       PIC X(15)         VALUE
                "Lectura exitosa".

           05 WSC-SUC-MSG-CUR        PIC X(27)         VALUE
                "Operacion en cursor exitosa".

      *****************************************************
      * Print format for the report
      *****************************************************
       01 BREAKLINE.
          05 BLANK-SPACE            PIC X(50)         VALUE ALL " ".

       01 HEADER-1.
          05 FILLER                 PIC X(09)         VALUE 'Realizo: '
           .
          05 WSV-AUTHOR             PIC X(06).

       01 HEADER-2.
          05 FILLER                 PIC X(06)         VALUE
                'Nombre'.
          05 FILLER                 PIC X(09)         VALUE SPACES.
          05 FILLER                 PIC X(06)         VALUE
                'Estado'.
          05 FILLER                 PIC X(12)         VALUE SPACES.

          05 FILLER                 PIC X(05)         VALUE
                'Saldo'.


       01 FOOTER-LINES.
          05 FILLER                 PIC X(44)         VALUE ALL "-".

       01 FOOTER-DATA-1.
          05 FILLER                 PIC X(33)         VALUE
                'Promedio de saldo de clientes es:'.
          05 WSV-TOTAL-AVG-O        PIC $$,$$$,$$9.99.

      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
       EMPIEZO-PROGRAMA.
           OPEN OUTPUT REPOUT.
           SET NO-FIN-CURSOR TO TRUE
           PERFORM PROCESO-PRINCIPAL.

       FIN-PROGRAMA.
           CLOSE REPOUT.
           GOBACK.


       PROCESO-PRINCIPAL.
           PERFORM ESCRIBE-ENCABEZADO
      *
           PERFORM ABRO-CURSOR
           PERFORM UPDATE-RECORDS UNTIL FIN-CURSOR
           PERFORM CIERRO-CURSOR
           SET NO-FIN-CURSOR TO TRUE

      *
           PERFORM ABRO-CURSOR
           PERFORM LEO-CURSOR

           IF SQLCODE = SQLCODE100
              MOVE 'PROCESO-PRINCIPAL' TO WSV-PARG-NAME
              DISPLAY WSC-ERR-MSG-EMPTY ' en ' WSV-PARG-NAME ' SQLCODE:'
              SQLCODE

              STOP RUN
           END-IF

      *
           PERFORM CICLO-CURSOR UNTIL FIN-CURSOR
           PERFORM CIERRO-CURSOR
           PERFORM CALCULATE-AVG

           PERFORM ESCRIBE-FOOTER.



       CICLO-CURSOR.
           PERFORM ESCRIBE-REPORTE.
           PERFORM LEO-CURSOR.



       ABRO-CURSOR.
           EXEC SQL
                OPEN CURTABLA
           END-EXEC.

            MOVE 'ABRO-CURSOR' TO WSV-PARG-NAME

           PERFORM QMLOG-OPEN-CURSOR.



       LEO-CURSOR.
           EXEC SQL
                FETCH CURTABLA
                INTO :VARIABLES-HOST
           END-EXEC

           MOVE 'LEO-CURSOR' TO WSV-PARG-NAME

           PERFORM QMLOG-READ-CURSOR.



       CIERRO-CURSOR.
           EXEC SQL
                CLOSE CURTABLA
           END-EXEC.

           MOVE "CIERRO-CURSOR." TO WSV-PARG-NAME

           PERFORM QMLOG-CLOSE-CURSOR .


      ***************************************************
      * Paragraphs to print out the report of the data  *
      ***************************************************
       ESCRIBO-AUTOR.
           MOVE 'Victor' TO WSV-AUTHOR.
           WRITE REPREC FROM HEADER-1.


       ESCRIBE-REPORTE.
           INITIALIZE REPREC
           MOVE ACCT-NOMBRE TO ACCT-NOMBRE-O
           MOVE ACCT-DIRE3 TO ACCT-STATE-O
           MOVE ACCT-SALDO TO ACCT-SALDO-O
           WRITE REPREC AFTER ADVANCING 1 LINES.

       ESCRIBE-ENCABEZADO.
           PERFORM ESCRIBO-AUTOR
           WRITE REPREC FROM BREAKLINE AFTER ADVANCING 1 LINES
           WRITE REPREC FROM HEADER-2
           WRITE REPREC FROM BREAKLINE.

       ESCRIBE-FOOTER.
           MOVE WSV-TOTAL-AVG TO WSV-TOTAL-AVG-O
           WRITE REPREC FROM BREAKLINE
           WRITE REPREC FROM FOOTER-LINES AFTER ADVANCING 1 LINES
           WRITE REPREC FROM FOOTER-DATA-1.

      ****************************************************
      * The EVALUATE-SQLCODES subroutine evaluates the   *
      * SQLCODE value and sets flags accordingly.        *
      ****************************************************
       EVALUATE-SQLCODES.

           EVALUATE SQLCODE
               WHEN SQLCODE0
                    SET NO-FIN-CURSOR TO TRUE
               WHEN SQLCODE100
                    SET FIN-CURSOR TO TRUE
                    MOVE 'EVALUATE-SQLCODES' TO WSV-PARG-NAME
                    DISPLAY WSC-ERR-MSG-EMPTY ' al ejecutar '
                    WSV-PARG-NAME ' - SQLCIDE:' SQLCODE
               WHEN OTHER
                    PERFORM SQL-ROLLBACK
                    DISPLAY WSC-SUC-MSG-ROLLBACK ' al ejecutar '
                    WSV-PARG-NAME ' - SQLCIDE:' SQLCODE
                    STOP RUN
           END-EVALUATE.

      ****************************************************
      * Root function that calls the other function below*
      ****************************************************
       UPDATE-RECORDS.
           PERFORM LEO-CURSOR

           IF SQLCODE = 0
              PERFORM UPDATE-NY-RECORD
           END-IF.


      ****************************************************
      * Update the state to New York to the best clients *
      ****************************************************
       UPDATE-NY-RECORD.
           EXEC SQL
                UPDATE Z94474T
                SET ADDRESS3 = :WSC-UPD-NY-STATE
                WHERE BALANCE IN (
                SELECT BALANCE
                FROM Z94474T
                ORDER BY BALANCE DESC
                FETCH FIRST 3 ROWS ONLY)
           END-EXEC

           MOVE "UPDATE-NY-RECORD" TO WSV-PARG-NAME

           PERFORM QMLOG-UPDATE.

      ****************************************************
      * Calculates the average of all the balances       *
      ****************************************************

       CALCULATE-AVG.
           EXEC SQL
                SELECT AVG(BALANCE)
                INTO :WSV-TOTAL-AVG
                FROM Z94474T
           END-EXEC

           MOVE "CALCULATE-AVG" TO WSV-PARG-NAME

           PERFORM QMLOG-READ

           PERFORM SQL-COMMIT

           MOVE WSV-TOTAL-AVG TO WSV-TOTAL-AVG-O.


      ****************************************************
      * SQL-COMMIT procedure to commit the changes       *
      ****************************************************
       SQL-COMMIT.
           EXEC SQL
                 COMMIT
           END-EXEC.


      ****************************************************
      * SQL-ROLLBACK procedure to restore data           *
      ****************************************************
       SQL-ROLLBACK.
           EXEC SQL
                 ROLLBACK
           END-EXEC.


      ****************************************************
      * Functions to print a log of the program running  *
      ****************************************************

       QMLOG-READ.
           EVALUATE SQLCODE
                WHEN SQLCODE0
                     DISPLAY WSC-SUC-MSG-READ ' en ' WSV-PARG-NAME
                WHEN OTHER
                     DISPLAY WSC-ERR-MSG-PARG ' - ' WSV-PARG-NAME
                     PERFORM EVALUATE-SQLCODES
           END-EVALUATE.



       QMLOG-UPDATE.
           EVALUATE SQLCODE
               WHEN SQLCODE0
                    DISPLAY WSC-SUC-MSG-UPD ' en ' WSV-PARG-NAME
                    ' - SQLCODE: ' SQLCODE
               WHEN OTHER
                    DISPLAY WSC-ERR-MSG-PARG ' ' WSV-PARG-NAME ' '
                    PERFORM EVALUATE-SQLCODES
           END-EVALUATE.


       QMLOG-READ-CURSOR.
           EVALUATE SQLCODE
               WHEN SQLCODE0
                    DISPLAY WSC-SUC-MSG-CUR ' en ' WSV-PARG-NAME
               WHEN OTHER
                    DISPLAY WSC-ERR-MSG-CUR ' ' WSV-PARG-NAME
                    PERFORM EVALUATE-SQLCODES
           END-EVALUATE.



       QMLOG-OPEN-CURSOR.
           EVALUATE SQLCODE
               WHEN SQLCODE0
                    DISPLAY WSC-SUC-MSG-CUR ' en ' WSV-PARG-NAME
               WHEN OTHER
                    DISPLAY WSC-ERR-MSG-CUR ' ' WSV-PARG-NAME
                    PERFORM EVALUATE-SQLCODES
           END-EVALUATE.



       QMLOG-CLOSE-CURSOR.
           EVALUATE SQLCODE
               WHEN SQLCODE0
                    DISPLAY WSC-SUC-MSG-CUR ' en ' WSV-PARG-NAME
               WHEN OTHER
                    DISPLAY WSC-ERR-MSG-CUR ' ' WSV-PARG-NAME
                    PERFORM EVALUATE-SQLCODES
           END-EVALUATE.





